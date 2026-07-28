use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use anyhow::{anyhow, bail, Result};
use ed25519_dalek::{Signature, Verifier, VerifyingKey};

use crate::cas::DiskCas;
use crate::digest::Digest;
use crate::model::{
    as_i64_usize, parse_chain_file, proposal_value_digest, quorum_size, signed_msg_payload_for_commit,
    valid_replica_count, Artifact, Block, FederationFinalityCertificate, FederationProposal, FederationState,
    FederationTransition, ReplicaSetManifest, Tx,
};

#[derive(Clone, Debug)]
pub struct HistoryReport {
    pub verified_transitions: usize,
    pub final_state: Digest,
    pub final_epoch: i64,
}

fn read_artifact(cas: &DiskCas, digest: Digest) -> Result<Artifact> {
    let bs = cas.read_blob(digest)?;
    let a = Artifact::decode(&bs)?;
    Ok(a)
}

fn verify_ed25519(pk: &[u8], payload: &[u8], seal: &[u8]) -> Result<bool> {
    if pk.len() != 32 || seal.len() != 64 {
        return Ok(false);
    }
    let mut pk_arr = [0u8; 32];
    pk_arr.copy_from_slice(pk);
    let mut sig_arr = [0u8; 64];
    sig_arr.copy_from_slice(seal);
    let vk = VerifyingKey::from_bytes(&pk_arr)?;
    let sig = Signature::from_bytes(&sig_arr);
    Ok(vk.verify(payload, &sig).is_ok())
}

fn verify_manifest_seals(manifest: &ReplicaSetManifest, manifest_artifact: &Artifact) -> Result<()> {
    let body = manifest_artifact
        .body
        .expect_tag("replica-set-manifest")?
        .field("body")?
        .encode();

    let ids: BTreeSet<String> = manifest.authorities.iter().map(|(id, _)| id.clone()).collect();
    let seal_ids: BTreeSet<String> = manifest.seals.iter().map(|(id, _)| id.clone()).collect();
    if ids.is_empty() {
        bail!("replica-set: empty");
    }
    if ids != seal_ids {
        bail!("replica-set: seal coverage incomplete");
    }

    let auth = manifest.authority_map();
    for (id, seal) in &manifest.seals {
        let pk = auth
            .get(id)
            .ok_or_else(|| anyhow!("replica-set: seal for unknown id '{}'", id))?;
        if !verify_ed25519(pk, &body, seal)? {
            bail!("replica-set: bad seal from '{}'", id);
        }
    }
    Ok(())
}

fn verify_federation_cert_quorum(
    cert: &FederationFinalityCertificate,
    authorities: &BTreeMap<String, Vec<u8>>,
    expected_replica_set: Digest,
) -> Result<()> {
    let n = authorities.len();
    if !valid_replica_count(n) {
        bail!("federation finality: n={} is not a valid 3f+1 size", n);
    }
    if cert.seq != cert.epoch {
        bail!(
            "federation finality: certificate sequence {} does not equal epoch {}",
            cert.seq,
            cert.epoch
        );
    }
    if cert.replica_set != expected_replica_set {
        bail!(
            "federation finality: replicaSet {} != expected {}",
            cert.replica_set.short(),
            expected_replica_set.short()
        );
    }

    let commit_ids: Vec<&str> = cert.commits.iter().map(|c| c.replica.as_str()).collect();
    let distinct: BTreeSet<&str> = commit_ids.iter().copied().collect();
    if distinct.len() != commit_ids.len() {
        bail!("federation finality: duplicate replica commits");
    }
    for id in &distinct {
        if !authorities.contains_key(*id) {
            bail!("federation finality: unknown replica in commits");
        }
    }
    let q = quorum_size(n);
    if distinct.len() < q {
        bail!(
            "federation finality: {} distinct commits < quorum {}",
            distinct.len(),
            q
        );
    }

    let value_digest = proposal_value_digest(cert.proposal);
    for c in &cert.commits {
        let payload = signed_msg_payload_for_commit(
            cert.view,
            cert.seq,
            value_digest,
            &c.replica,
            cert.replica_set,
            cert.federation_id,
        );
        let pk = authorities
            .get(&c.replica)
            .ok_or_else(|| anyhow!("unknown replica {}", c.replica))?;
        if !verify_ed25519(pk, &payload, &c.seal)? {
            bail!("bad bft seal from {}", c.replica);
        }
    }

    Ok(())
}

fn verify_cert_for_proposal(
    cert: &FederationFinalityCertificate,
    proposal: &FederationProposal,
    manifest: &ReplicaSetManifest,
) -> Result<()> {
    if cert.proposal != proposal.digest() {
        bail!(
            "federation finality: certificate names proposal {}, not {}",
            cert.proposal.short(),
            proposal.digest().short()
        );
    }
    verify_federation_cert_quorum(cert, &manifest.authority_map(), manifest.replica_set_digest)?;

    if cert.transition != proposal.transition {
        bail!("federation finality: certificate transition projection does not match the signed proposal");
    }
    if cert.state_digest != proposal.after {
        bail!("federation finality: certificate state projection does not match the signed proposal's after-state");
    }
    if cert.previous_state != proposal.before {
        bail!(
            "federation finality: certificate previousState projection does not match the signed proposal's before-state"
        );
    }
    if cert.epoch != proposal.epoch {
        bail!("federation finality: certificate epoch projection does not match the signed proposal");
    }
    if cert.replica_set != proposal.replica_set {
        bail!("federation finality: certificate replicaSet projection does not match the signed proposal");
    }
    if cert.federation_id != proposal.federation_id {
        bail!("federation finality: certificate federationId projection does not match the signed proposal");
    }
    Ok(())
}

pub fn verify_cert_command(
    cas_root: &str,
    cert_digest: Digest,
    proposal_digest: Digest,
    manifest_digest: Digest,
) -> Result<()> {
    let cas = DiskCas::new(cas_root);
    let cert_artifact = read_artifact(&cas, cert_digest)?;
    let cert = FederationFinalityCertificate::from_artifact(&cert_artifact)?;

    let proposal_artifact = read_artifact(&cas, proposal_digest)?;
    let proposal = FederationProposal::from_artifact(&proposal_artifact)?;

    let manifest_artifact = read_artifact(&cas, manifest_digest)?;
    let manifest = ReplicaSetManifest::from_artifact(&manifest_artifact)?;
    verify_manifest_seals(&manifest, &manifest_artifact)?;

    verify_cert_for_proposal(&cert, &proposal, &manifest)
}

fn collect_federation_artifacts_from_chain(chain: &[Block]) -> (Vec<Digest>, BTreeSet<Digest>) {
    let mut transitions = Vec::new();
    let mut recorded_certs = BTreeSet::new();
    for b in chain {
        for stx in &b.txs {
            match &stx.tx {
                Tx::PublishArtifact(k) if k.kind == "federation-transition" => transitions.push(k.value_hash),
                Tx::RecordCertificate(d, method) if method == "federation-finality" => {
                    recorded_certs.insert(*d);
                }
                _ => {}
            }
        }
    }
    (transitions, recorded_certs)
}

pub fn verify_history_command_with_limit(
    node_root: &str,
    expected_federation_id: Digest,
    expected_genesis_state: Digest,
    max_steps: Option<usize>,
) -> Result<HistoryReport> {
    let root = Path::new(node_root);
    let cas = DiskCas::new(root);

    let chain_text = cas.read_text_file(root.join("chain"))?;
    let chain_digests = parse_chain_file(&chain_text)?;
    if chain_digests.is_empty() {
        bail!("empty chain");
    }

    let mut chain = Vec::with_capacity(chain_digests.len());
    for d in &chain_digests {
        let a = read_artifact(&cas, *d)?;
        let block = Block::from_artifact(&a)?;
        if block.digest != *d {
            bail!("block digest mismatch {}", d.short());
        }
        chain.push(block);
    }

    let (transition_digests, recorded_certs) = collect_federation_artifacts_from_chain(&chain);
    if transition_digests.is_empty() {
        bail!("no federation transitions published on chain");
    }
    if let Some(limit) = max_steps {
        if transition_digests.len() > limit {
            bail!(
                "kernel exhausted: max_steps {} exceeded by {} transitions",
                limit,
                transition_digests.len()
            );
        }
    }

    let mut expected_before = expected_genesis_state;
    let mut last_state = expected_genesis_state;
    let mut last_epoch = 0i64;

    for td in &transition_digests {
        let transition_artifact = read_artifact(&cas, *td)?;
        let transition = FederationTransition::from_artifact(&transition_artifact)?;

        if transition.before != expected_before {
            bail!(
                "federation history: transition {} does not chain from {}",
                td.short(),
                expected_before.short()
            );
        }

        let cert_digest = transition
            .finality
            .ok_or_else(|| anyhow!("federation history: transition {} missing finality", td.short()))?;
        if !recorded_certs.contains(&cert_digest) {
            bail!(
                "federation history: certificate {} not anchored in ledger record-certificate txs",
                cert_digest.short()
            );
        }

        let cert_artifact = read_artifact(&cas, cert_digest)?;
        let cert = FederationFinalityCertificate::from_artifact(&cert_artifact)?;
        if cert.federation_id != expected_federation_id {
            bail!("federation finality: federation id mismatch");
        }

        let proposal_artifact = read_artifact(&cas, cert.proposal)?;
        let proposal = FederationProposal::from_artifact(&proposal_artifact)?;

        let before_state_artifact = read_artifact(&cas, transition.before)?;
        let before_state = FederationState::from_artifact(&before_state_artifact)?;
        let after_state_artifact = read_artifact(&cas, transition.after)?;
        let _after_state = FederationState::from_artifact(&after_state_artifact)?;

        let manifest_artifact = read_artifact(&cas, before_state.trust_roots)?;
        let manifest = ReplicaSetManifest::from_artifact(&manifest_artifact)?;
        verify_manifest_seals(&manifest, &manifest_artifact)?;

        verify_cert_for_proposal(&cert, &proposal, &manifest)?;

        if proposal.transition != *td {
            bail!(
                "federation history: proposal {} transition does not match published transition {}",
                cert.proposal.short(),
                td.short()
            );
        }
        if proposal.before != transition.before || proposal.after != transition.after {
            bail!(
                "federation history: proposal {} before/after does not match transition {}",
                cert.proposal.short(),
                td.short()
            );
        }

        if cert.state_digest != transition.after {
            bail!(
                "federation finality: certificate {} subject is not transition.after",
                cert_digest.short()
            );
        }
        if cert.previous_state != transition.before {
            bail!(
                "federation finality: certificate {} predecessor is not transition.before",
                cert_digest.short()
            );
        }

        expected_before = transition.after;
        last_state = transition.after;
        last_epoch = cert.epoch;
        let _ = as_i64_usize(last_epoch, "epoch")?;
    }

    Ok(HistoryReport {
        verified_transitions: transition_digests.len(),
        final_state: last_state,
        final_epoch: last_epoch,
    })
}
