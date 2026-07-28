use std::collections::{BTreeMap, BTreeSet};

use anyhow::Result;

use crate::cas::DiskCas;
use crate::canon::Canon;
use crate::digest::Digest;
use crate::model::{Artifact, FederationFinalityCertificate};
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

#[derive(Clone, Debug)]
pub struct CertificateProjection {
    pub proposal: Digest,
    pub manifest: Digest,
}

#[derive(Clone, Debug)]
pub struct Transition {
    pub transition: Digest,
    pub before: Digest,
    pub after: Digest,
    pub cert: Digest,
    pub proposal: Digest,
    pub manifest: Digest,
    pub federation_id: Digest,
    pub epoch: i64,
}

#[derive(Clone, Debug, Default)]
pub struct Context {
    pub resolved_artifacts: BTreeMap<Digest, Artifact>,
    pub artifact_closure: BTreeSet<Digest>,
    pub certs: BTreeMap<Digest, CertificateProjection>,
    pub history: Vec<Transition>,
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

fn missing_closure_for_history(ctx: &Context) -> BTreeSet<Digest> {
    let mut missing = BTreeSet::new();
    for t in &ctx.history {
        if !ctx.artifact_closure.contains(&t.transition) {
            missing.insert(t.transition);
        }
        if !ctx.certs.contains_key(&t.cert) {
            missing.insert(t.cert);
        }
        if !ctx.artifact_closure.contains(&t.proposal) {
            missing.insert(t.proposal);
        }
        if !ctx.artifact_closure.contains(&t.manifest) {
            missing.insert(t.manifest);
        }
    }
    missing
}

fn replay_transitions(
    federation_id: Digest,
    state: Digest,
    last_epoch: i64,
    xs: &[Transition],
) -> Result<(Digest, i64, usize)> {
    if xs.is_empty() {
        return Ok((state, last_epoch, 0));
    }

    let t = &xs[0];
    if t.federation_id != federation_id {
        anyhow::bail!("federation id mismatch at epoch {}", t.epoch);
    }
    if t.before != state {
        anyhow::bail!(
            "transition chain break: expected before={}, got {}",
            state.hex(),
            t.before.hex()
        );
    }
    if t.epoch < last_epoch {
        anyhow::bail!("epoch regression: {} < {}", t.epoch, last_epoch);
    }

    let (final_state, final_epoch, n) = replay_transitions(federation_id, t.after, t.epoch, &xs[1..])?;
    Ok((final_state, final_epoch, n + 1))
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
                let mut missing = BTreeSet::new();
                if !ctx.certs.contains_key(&cert) {
                    missing.insert(cert);
                }
                if !ctx.artifact_closure.contains(&proposal) {
                    missing.insert(proposal);
                }
                if !ctx.artifact_closure.contains(&manifest) {
                    missing.insert(manifest);
                }
                if !missing.is_empty() {
                    anyhow::bail!("artifact not in CAS: {:?}", missing);
                }

                let cp = ctx
                    .certs
                    .get(&cert)
                    .ok_or_else(|| anyhow::anyhow!("certificate not in CAS: {}", cert.hex()))?;

                if cp.proposal != proposal {
                    anyhow::bail!("certificate/proposal mismatch");
                }
                if cp.manifest != manifest {
                    anyhow::bail!("certificate/manifest mismatch");
                }

                Ok(Value::CertBinding {
                    cert,
                    proposal,
                    manifest,
                })
            }
            SemanticQuery::ReplayHistory {
                federation_id,
                genesis_state,
            } => {
                if ctx.history.len() > budget.max_steps {
                    anyhow::bail!(
                        "kernel exhausted: max_steps {} exceeded by {} transitions",
                        budget.max_steps,
                        ctx.history.len()
                    );
                }
                if ctx.history.is_empty() {
                    anyhow::bail!("no federation transitions published on chain");
                }

                let miss = missing_closure_for_history(ctx);
                if !miss.is_empty() {
                    anyhow::bail!("artifact not in CAS: {:?}", miss);
                }

                let (final_state, final_epoch, verified_transitions) =
                    replay_transitions(federation_id, genesis_state, 0, &ctx.history)?;
                Ok(Value::ReplayedState(HistoryReport {
                    verified_transitions,
                    final_state,
                    final_epoch,
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
    ctx.artifact_closure.insert(digest);
    Ok(ctx)
}

fn load_context_for_verify_cert(
    cas_root: &str,
    cert: Digest,
    proposal: Digest,
    manifest: Digest,
) -> Result<Context> {
    verify::verify_cert_command(cas_root, cert, proposal, manifest)?;

    let cas = DiskCas::new(cas_root);
    let cert_artifact = cas.read_blob(cert).and_then(|bs| Artifact::decode(&bs))?;
    let cert_view = FederationFinalityCertificate::from_artifact(&cert_artifact)?;

    let mut ctx = Context::default();
    ctx.artifact_closure.insert(proposal);
    ctx.artifact_closure.insert(manifest);
    ctx.artifact_closure.insert(cert);
    ctx.certs.insert(
        cert,
        CertificateProjection {
            proposal: cert_view.proposal,
            manifest,
        },
    );
    Ok(ctx)
}

fn load_context_for_replay(
    node_root: &str,
    federation_id: Digest,
    genesis_state: Digest,
    budget: Budget,
) -> Result<Context> {
    let loaded = verify::build_verified_history_context_with_limit(
        node_root,
        federation_id,
        genesis_state,
        Some(budget.max_steps),
    )?;

    let mut ctx = Context::default();
    for t in loaded.transitions {
        ctx.artifact_closure.insert(t.transition);
        ctx.artifact_closure.insert(t.before);
        ctx.artifact_closure.insert(t.after);
        ctx.artifact_closure.insert(t.cert);
        ctx.artifact_closure.insert(t.proposal);
        ctx.artifact_closure.insert(t.manifest);
        ctx.certs.insert(
            t.cert,
            CertificateProjection {
                proposal: t.proposal,
                manifest: t.manifest,
            },
        );
        ctx.history.push(Transition {
            transition: t.transition,
            before: t.before,
            after: t.after,
            cert: t.cert,
            proposal: t.proposal,
            manifest: t.manifest,
            federation_id: t.federation_id,
            epoch: t.epoch,
        });
    }
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
        Ok((ctx, semantic_query)) => derive_semantic(&ctx, constitution, budget, semantic_query),
        Err(e) => classify_error(e.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classify_exhausted_error() {
        match classify_error("kernel exhausted: max_steps 10 exceeded".to_owned()) {
            KernelResult::Exhausted { limit } => assert!(limit.contains("max_steps")),
            other => panic!("expected exhausted, got {:?}", other),
        }
    }

    #[test]
    fn evidence_is_deterministic_for_same_inputs() {
        let k = KernelConstitution::default();
        let d = Digest::of_bytes(b"x");
        let q = SemanticQuery::VerifyCertBinding {
            cert: d,
            proposal: d,
            manifest: d,
        };
        let v = Value::CertBinding {
            cert: d,
            proposal: d,
            manifest: d,
        };
        let e1 = evidence_of(&k, &q, &v);
        let e2 = evidence_of(&k, &q, &v);
        assert_eq!(e1, e2);
    }
}
