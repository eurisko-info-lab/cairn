use std::collections::{BTreeMap, BTreeSet};

use anyhow::Result;

use crate::cas::DiskCas;
use crate::canon::Canon;
use crate::digest::Digest;
use crate::model::Artifact;
use crate::verify::{self, HistoryReport};

#[derive(Clone, Debug)]
pub struct KernelConstitution {
    pub kernel_id: String,
}

impl Default for KernelConstitution {
    fn default() -> Self {
        Self {
            kernel_id: "ckc-v0".to_owned(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Budget {
    pub max_steps: usize,
}

impl Default for Budget {
    fn default() -> Self {
        Self { max_steps: 100_000 }
    }
}

#[derive(Clone, Debug)]
pub enum Query {
    Resolve {
        cas_root: String,
        digest: Digest,
    },
    VerifyCertBinding {
        cas_root: String,
        cert: Digest,
        proposal: Digest,
        manifest: Digest,
    },
    ReplayHistory {
        node_root: String,
        federation_id: Digest,
        genesis_state: Digest,
    },
}

#[derive(Clone, Debug)]
pub enum SemanticQuery {
    Resolve { digest: Digest },
    VerifyCertBinding {
        cert: Digest,
        proposal: Digest,
        manifest: Digest,
    },
    ReplayHistory {
        federation_id: Digest,
        genesis_state: Digest,
    },
}

#[derive(Clone, Debug, Default)]
pub struct Context {
    pub resolved_artifacts: BTreeMap<Digest, Artifact>,
    pub verify_cert_cas_root: Option<String>,
    pub replay_node_root: Option<String>,
}

#[derive(Clone, Debug)]
pub enum Value {
    Artifact(Artifact),
    CertBinding {
        cert: Digest,
        proposal: Digest,
        manifest: Digest,
    },
    ReplayedState(HistoryReport),
}

#[derive(Clone, Debug)]
pub enum KernelResult {
    Valid { value: Value, evidence: Digest },
    Invalid { error: String },
    Missing { closure: BTreeSet<Digest> },
    Exhausted { limit: String },
}

fn evidence_of(constitution: &KernelConstitution, query: &SemanticQuery, value: &Value) -> Digest {
    let qcanon = match query {
        SemanticQuery::Resolve { digest } => Canon::Tag(
            "resolve".to_owned(),
            Box::new(Canon::Str(digest.hex())),
        ),
        SemanticQuery::VerifyCertBinding {
            cert,
            proposal,
            manifest,
        } => Canon::Tag(
            "verify-cert-binding".to_owned(),
            Box::new(Canon::Map(vec![
                ("cert".to_owned(), Canon::Str(cert.hex())),
                ("proposal".to_owned(), Canon::Str(proposal.hex())),
                ("manifest".to_owned(), Canon::Str(manifest.hex())),
            ])),
        ),
        SemanticQuery::ReplayHistory {
            federation_id,
            genesis_state,
        } => Canon::Tag(
            "replay-history".to_owned(),
            Box::new(Canon::Map(vec![
                ("federationId".to_owned(), Canon::Str(federation_id.hex())),
                ("genesisState".to_owned(), Canon::Str(genesis_state.hex())),
            ])),
        ),
    };

    let vcanon = match value {
        Value::Artifact(a) => Canon::Tag(
            "artifact".to_owned(),
            Box::new(Canon::Str(a.digest().hex())),
        ),
        Value::CertBinding {
            cert,
            proposal,
            manifest,
        } => Canon::Tag(
            "cert-binding".to_owned(),
            Box::new(Canon::Map(vec![
                ("cert".to_owned(), Canon::Str(cert.hex())),
                ("proposal".to_owned(), Canon::Str(proposal.hex())),
                ("manifest".to_owned(), Canon::Str(manifest.hex())),
            ])),
        ),
        Value::ReplayedState(report) => Canon::Tag(
            "replayed-state".to_owned(),
            Box::new(Canon::Map(vec![
                (
                    "verifiedTransitions".to_owned(),
                    Canon::Int(report.verified_transitions as i64),
                ),
                ("finalState".to_owned(), Canon::Str(report.final_state.hex())),
                ("finalEpoch".to_owned(), Canon::Int(report.final_epoch)),
            ])),
        ),
    };

    Digest::of_bytes(
        &Canon::Map(vec![
            (
                "kernelId".to_owned(),
                Canon::Str(constitution.kernel_id.clone()),
            ),
            ("query".to_owned(), qcanon),
            ("value".to_owned(), vcanon),
        ])
        .encode(),
    )
}

fn classify_error(error: String) -> KernelResult {
    if let Some(rest) = error.strip_prefix("kernel exhausted:") {
        return KernelResult::Exhausted {
            limit: rest.trim().to_owned(),
        };
    }

    let mut closure = BTreeSet::new();
    for token in error.split(|c: char| !c.is_ascii_hexdigit()) {
        if token.len() == 64 {
            if let Ok(d) = Digest::parse(token) {
                closure.insert(d);
            }
        }
    }
    if error.contains("not in CAS") {
        return KernelResult::Missing { closure };
    }

    KernelResult::Invalid { error }
}

pub fn derive_semantic(
    ctx: &Context,
    constitution: &KernelConstitution,
    budget: Budget,
    query: SemanticQuery,
) -> KernelResult {
    let evaluated: Result<Value> = (|| {
        match query {
            SemanticQuery::Resolve { digest } => match ctx.resolved_artifacts.get(&digest) {
                Some(artifact) => Ok(Value::Artifact(artifact.clone())),
                None => Err(anyhow::anyhow!("artifact not in CAS: {}", digest.hex())),
            },
            SemanticQuery::VerifyCertBinding {
                cert,
                proposal,
                manifest,
            } => {
                let root = ctx
                    .verify_cert_cas_root
                    .as_ref()
                    .ok_or_else(|| anyhow::anyhow!("missing verify-cert loader context"))?;
                verify::verify_cert_command(root, cert, proposal, manifest)?;
                Ok(Value::CertBinding { cert, proposal, manifest })
            }
            SemanticQuery::ReplayHistory {
                federation_id,
                genesis_state,
            } => {
                let root = ctx
                    .replay_node_root
                    .as_ref()
                    .ok_or_else(|| anyhow::anyhow!("missing replay loader context"))?;
                let loaded = verify::build_verified_history_context_with_limit(
                    root,
                    federation_id,
                    genesis_state,
                    Some(budget.max_steps),
                )?;
                let final_transition = loaded
                    .transitions
                    .last()
                    .ok_or_else(|| anyhow::anyhow!("no federation transitions published on chain"))?;
                Ok(Value::ReplayedState(HistoryReport {
                    verified_transitions: loaded.transitions.len(),
                    final_state: final_transition.after,
                    final_epoch: final_transition.epoch,
                }))
            }
        }
    })();

    match evaluated {
        Ok(value) => {
            let evidence = evidence_of(constitution, &query, &value);
            KernelResult::Valid { value, evidence }
        }
        Err(e) => classify_error(e.to_string()),
    }
}

fn load_context_for_resolve(cas_root: &str, digest: Digest) -> Result<Context> {
    let cas = DiskCas::new(cas_root);
    let artifact = cas
        .read_blob(digest)
        .and_then(|bs| Artifact::decode(&bs))?;

    let mut ctx = Context::default();
    ctx.resolved_artifacts.insert(digest, artifact);
    Ok(ctx)
}

fn load_context_for_verify_cert(
    cas_root: &str,
    cert: Digest,
    proposal: Digest,
    manifest: Digest,
) -> Result<Context> {
    let cas = DiskCas::new(cas_root);
    let cert_artifact = cas.read_blob(cert).and_then(|bs| Artifact::decode(&bs))?;
    let proposal_artifact = cas.read_blob(proposal).and_then(|bs| Artifact::decode(&bs))?;
    let manifest_artifact = cas.read_blob(manifest).and_then(|bs| Artifact::decode(&bs))?;

    let mut ctx = Context::default();
    ctx.resolved_artifacts.insert(cert, cert_artifact);
    ctx.resolved_artifacts.insert(proposal, proposal_artifact);
    ctx.resolved_artifacts.insert(manifest, manifest_artifact);
    ctx.verify_cert_cas_root = Some(cas_root.to_owned());
    Ok(ctx)
}

fn load_context_for_replay(
    node_root: &str,
    federation_id: Digest,
    genesis_state: Digest,
    budget: Budget,
) -> Result<Context> {
    let mut ctx = Context::default();
    let _ = (federation_id, genesis_state, budget);
    ctx.replay_node_root = Some(node_root.to_owned());
    Ok(ctx)
}

pub fn derive(constitution: &KernelConstitution, budget: Budget, query: Query) -> KernelResult {
    let loaded: Result<(Context, SemanticQuery)> = match query {
        Query::Resolve { cas_root, digest } => {
            load_context_for_resolve(&cas_root, digest).map(|ctx| (ctx, SemanticQuery::Resolve { digest }))
        }
        Query::VerifyCertBinding {
            cas_root,
            cert,
            proposal,
            manifest,
        } => load_context_for_verify_cert(&cas_root, cert, proposal, manifest).map(|ctx| {
            (
                ctx,
                SemanticQuery::VerifyCertBinding {
                    cert,
                    proposal,
                    manifest,
                },
            )
        }),
        Query::ReplayHistory {
            node_root,
            federation_id,
            genesis_state,
        } => load_context_for_replay(&node_root, federation_id, genesis_state, budget).map(|ctx| {
            (
                ctx,
                SemanticQuery::ReplayHistory {
                    federation_id,
                    genesis_state,
                },
            )
        }),
    };

    match loaded {
        Ok((ctx, sq)) => derive_semantic(&ctx, constitution, budget, sq),
        Err(e) => classify_error(e.to_string()),
    }
}
