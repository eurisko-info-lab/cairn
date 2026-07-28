use std::collections::BTreeSet;

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

fn evidence_of(constitution: &KernelConstitution, query: &Query, value: &Value) -> Digest {
    let qcanon = match query {
        Query::Resolve { digest, .. } => Canon::Tag(
            "resolve".to_owned(),
            Box::new(Canon::Str(digest.hex())),
        ),
        Query::VerifyCertBinding {
            cert,
            proposal,
            manifest,
            ..
        } => Canon::Tag(
            "verify-cert-binding".to_owned(),
            Box::new(Canon::Map(vec![
                ("cert".to_owned(), Canon::Str(cert.hex())),
                ("proposal".to_owned(), Canon::Str(proposal.hex())),
                ("manifest".to_owned(), Canon::Str(manifest.hex())),
            ])),
        ),
        Query::ReplayHistory {
            federation_id,
            genesis_state,
            ..
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
    if !closure.is_empty() && error.contains("not in CAS") {
        return KernelResult::Missing { closure };
    }

    KernelResult::Invalid { error }
}

pub fn derive(constitution: &KernelConstitution, budget: Budget, query: Query) -> KernelResult {
    let evaluated: Result<Value> = match &query {
        Query::Resolve { cas_root, digest } => {
            let cas = DiskCas::new(cas_root);
            cas.read_blob(*digest)
                .and_then(|bs| Artifact::decode(&bs))
                .map(Value::Artifact)
        }
        Query::VerifyCertBinding {
            cas_root,
            cert,
            proposal,
            manifest,
        } => verify::verify_cert_command(cas_root, *cert, *proposal, *manifest).map(|_| {
            Value::CertBinding {
                cert: *cert,
                proposal: *proposal,
                manifest: *manifest,
            }
        }),
        Query::ReplayHistory {
            node_root,
            federation_id,
            genesis_state,
        } => verify::verify_history_command_with_limit(
            node_root,
            *federation_id,
            *genesis_state,
            Some(budget.max_steps),
        )
        .map(Value::ReplayedState),
    };

    match evaluated {
        Ok(value) => {
            let evidence = evidence_of(constitution, &query, &value);
            KernelResult::Valid { value, evidence }
        }
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
        let q = Query::VerifyCertBinding {
            cas_root: "/tmp/any".to_owned(),
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
