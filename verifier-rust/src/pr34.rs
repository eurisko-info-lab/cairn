use anyhow::{anyhow, bail, Result};

use crate::canon::Canon;
use crate::digest::Digest;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Pr34GraphPackage {
    pub kernel_constitution: Digest,
    pub artifact_closure: Digest,
    pub machine_closure: Digest,
    pub runtime_closure: Digest,
    pub acceptance_closure: Digest,
    pub repository_root: Digest,
    pub finalized_history: Digest,
    pub evidence_closure: Digest,
}

impl Pr34GraphPackage {
    pub fn canon(&self) -> Canon {
        Canon::Tag(
            "pr34-graph-package-v1".to_owned(),
            Box::new(Canon::Map(vec![
                (
                    "kernelConstitution".to_owned(),
                    Canon::Str(self.kernel_constitution.hex()),
                ),
                ("artifactClosure".to_owned(), Canon::Str(self.artifact_closure.hex())),
                ("machineClosure".to_owned(), Canon::Str(self.machine_closure.hex())),
                ("runtimeClosure".to_owned(), Canon::Str(self.runtime_closure.hex())),
                (
                    "acceptanceClosure".to_owned(),
                    Canon::Str(self.acceptance_closure.hex()),
                ),
                ("repositoryRoot".to_owned(), Canon::Str(self.repository_root.hex())),
                ("finalizedHistory".to_owned(), Canon::Str(self.finalized_history.hex())),
                ("evidenceClosure".to_owned(), Canon::Str(self.evidence_closure.hex())),
            ])),
        )
    }

    pub fn digest(&self) -> Digest {
        Digest::of_bytes(&self.canon().encode())
    }

    pub fn from_canon(c: &Canon) -> Result<Self> {
        let body = c.expect_tag("pr34-graph-package-v1")?;
        Ok(Self {
            kernel_constitution: Digest::parse(body.field("kernelConstitution")?.as_str()?)?,
            artifact_closure: Digest::parse(body.field("artifactClosure")?.as_str()?)?,
            machine_closure: Digest::parse(body.field("machineClosure")?.as_str()?)?,
            runtime_closure: Digest::parse(body.field("runtimeClosure")?.as_str()?)?,
            acceptance_closure: Digest::parse(body.field("acceptanceClosure")?.as_str()?)?,
            repository_root: Digest::parse(body.field("repositoryRoot")?.as_str()?)?,
            finalized_history: Digest::parse(body.field("finalizedHistory")?.as_str()?)?,
            evidence_closure: Digest::parse(body.field("evidenceClosure")?.as_str()?)?,
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Pr34VerdictClass {
    Valid,
    Invalid,
    Missing,
    Exhausted,
}

impl Pr34VerdictClass {
    pub fn wire(self) -> &'static str {
        match self {
            Self::Valid => "valid",
            Self::Invalid => "invalid",
            Self::Missing => "missing",
            Self::Exhausted => "exhausted",
        }
    }

    pub fn parse(s: &str) -> Result<Self> {
        match s {
            "valid" => Ok(Self::Valid),
            "invalid" => Ok(Self::Invalid),
            "missing" => Ok(Self::Missing),
            "exhausted" => Ok(Self::Exhausted),
            _ => bail!("invalid pr34 verdict class: {}", s),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Pr34ResourceUse {
    pub steps: i64,
    pub bytes_read: i64,
    pub wall_micros: i64,
}

impl Pr34ResourceUse {
    pub fn canon(self) -> Canon {
        Canon::Map(vec![
            ("steps".to_owned(), Canon::Int(self.steps)),
            ("bytesRead".to_owned(), Canon::Int(self.bytes_read)),
            ("wallMicros".to_owned(), Canon::Int(self.wall_micros)),
        ])
    }

    pub fn from_canon(c: &Canon) -> Result<Self> {
        Ok(Self {
            steps: c.field("steps")?.as_int()?,
            bytes_read: c.field("bytesRead")?.as_int()?,
            wall_micros: c.field("wallMicros")?.as_int()?,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Pr34VerdictEnvelope {
    pub kernel_constitution: Digest,
    pub graph_package: Digest,
    pub verdict_class: Pr34VerdictClass,
    pub state: Option<Digest>,
    pub evidence: Option<Digest>,
    pub resource_use: Pr34ResourceUse,
}

fn option_digest_to_canon(v: Option<Digest>) -> Canon {
    match v {
        None => Canon::Tag("none".to_owned(), Box::new(Canon::Int(0))),
        Some(d) => Canon::Tag("some".to_owned(), Box::new(Canon::Str(d.hex()))),
    }
}

fn option_digest_from_canon(c: &Canon) -> Result<Option<Digest>> {
    match c {
        Canon::Tag(tag, _) if tag == "none" => Ok(None),
        Canon::Tag(tag, value) if tag == "some" => {
            let h = value.as_str()?;
            Ok(Some(Digest::parse(h)?))
        }
        _ => Err(anyhow!("expected option digest")),
    }
}

impl Pr34VerdictEnvelope {
    pub fn canon(&self) -> Canon {
        Canon::Tag(
            "pr34-verdict-envelope-v1".to_owned(),
            Box::new(Canon::Map(vec![
                (
                    "kernelConstitution".to_owned(),
                    Canon::Str(self.kernel_constitution.hex()),
                ),
                ("graphPackage".to_owned(), Canon::Str(self.graph_package.hex())),
                (
                    "verdictClass".to_owned(),
                    Canon::Str(self.verdict_class.wire().to_owned()),
                ),
                ("state".to_owned(), option_digest_to_canon(self.state)),
                ("evidence".to_owned(), option_digest_to_canon(self.evidence)),
                ("resourceUse".to_owned(), self.resource_use.canon()),
            ])),
        )
    }

    pub fn digest(&self) -> Digest {
        Digest::of_bytes(&self.canon().encode())
    }

    pub fn from_canon(c: &Canon) -> Result<Self> {
        let body = c.expect_tag("pr34-verdict-envelope-v1")?;
        Ok(Self {
            kernel_constitution: Digest::parse(body.field("kernelConstitution")?.as_str()?)?,
            graph_package: Digest::parse(body.field("graphPackage")?.as_str()?)?,
            verdict_class: Pr34VerdictClass::parse(body.field("verdictClass")?.as_str()?)?,
            state: option_digest_from_canon(body.field("state")?)?,
            evidence: option_digest_from_canon(body.field("evidence")?)?,
            resource_use: Pr34ResourceUse::from_canon(body.field("resourceUse")?)?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn d(label: &str) -> Digest {
        Digest::of_bytes(label.as_bytes())
    }

    #[test]
    fn graph_package_round_trip() {
        let g = Pr34GraphPackage {
            kernel_constitution: d("k"),
            artifact_closure: d("sigma"),
            machine_closure: d("m"),
            runtime_closure: d("r"),
            acceptance_closure: d("rho"),
            repository_root: d("repo"),
            finalized_history: d("h"),
            evidence_closure: d("eta"),
        };
        let round = Pr34GraphPackage::from_canon(&g.canon()).unwrap();
        assert_eq!(round, g);
    }

    #[test]
    fn verdict_envelope_round_trip_and_determinism() {
        let v = Pr34VerdictEnvelope {
            kernel_constitution: d("k"),
            graph_package: d("g"),
            verdict_class: Pr34VerdictClass::Valid,
            state: Some(d("state")),
            evidence: Some(d("evidence")),
            resource_use: Pr34ResourceUse {
                steps: 1,
                bytes_read: 2,
                wall_micros: 3,
            },
        };
        let round = Pr34VerdictEnvelope::from_canon(&v.canon()).unwrap();
        assert_eq!(round, v);

        let b1 = v.canon().encode();
        let b2 = v.canon().encode();
        assert_eq!(b1, b2);
        assert_eq!(v.digest(), v.digest());
    }

    #[test]
    fn rejects_unknown_verdict_class() {
        let bad = Canon::Tag(
            "pr34-verdict-envelope-v1".to_owned(),
            Box::new(Canon::Map(vec![
                ("kernelConstitution".to_owned(), Canon::Str(d("k").hex())),
                ("graphPackage".to_owned(), Canon::Str(d("g").hex())),
                ("verdictClass".to_owned(), Canon::Str("mystery".to_owned())),
                ("state".to_owned(), Canon::Tag("none".to_owned(), Box::new(Canon::Int(0)))),
                ("evidence".to_owned(), Canon::Tag("none".to_owned(), Box::new(Canon::Int(0)))),
                (
                    "resourceUse".to_owned(),
                    Canon::Map(vec![
                        ("steps".to_owned(), Canon::Int(0)),
                        ("bytesRead".to_owned(), Canon::Int(0)),
                        ("wallMicros".to_owned(), Canon::Int(0)),
                    ]),
                ),
            ])),
        );
        assert!(Pr34VerdictEnvelope::from_canon(&bad).is_err());
    }
}
