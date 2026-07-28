use anyhow::{anyhow, bail, Result};

use crate::canon::Canon;
use crate::digest::Digest;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Artifact {
    pub kind: String,
    pub body: Canon,
}

impl Artifact {
    pub fn decode(bs: &[u8]) -> Result<Self> {
        let c = Canon::decode(bs)?;
        Self::from_canon(&c)
    }

    pub fn from_canon(c: &Canon) -> Result<Self> {
        let kind = c.field("kind")?.as_str()?.to_owned();
        let body = c.field("body")?.clone();
        Ok(Self { kind, body })
    }

    pub fn digest(&self) -> Digest {
        Digest::of_bytes(&self.canon().encode())
    }

    pub fn canon(&self) -> Canon {
        Canon::Map(vec![
            ("body".to_owned(), self.body.clone()),
            ("kind".to_owned(), Canon::Str(self.kind.clone())),
        ])
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SignedCommit {
    pub replica: String,
    pub seal: Vec<u8>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FederationProposal {
    pub federation_id: Digest,
    pub transition: Digest,
    pub before: Digest,
    pub after: Digest,
    pub epoch: i64,
    pub replica_set: Digest,
}

impl FederationProposal {
    pub fn from_artifact(a: &Artifact) -> Result<Self> {
        if a.kind != "federation-proposal" {
            bail!("artifact is not a federation proposal");
        }
        Self::from_canon(&a.body)
    }

    pub fn from_canon(c: &Canon) -> Result<Self> {
        let m = c.expect_tag("federation-proposal-v1")?;
        Ok(Self {
            federation_id: Digest::parse(m.field("federationId")?.as_str()?)?,
            transition: Digest::parse(m.field("transition")?.as_str()?)?,
            before: Digest::parse(m.field("before")?.as_str()?)?,
            after: Digest::parse(m.field("after")?.as_str()?)?,
            epoch: m.field("epoch")?.as_int()?,
            replica_set: Digest::parse(m.field("replicaSet")?.as_str()?)?,
        })
    }

    pub fn digest(&self) -> Digest {
        let body = Canon::Tag(
            "federation-proposal-v1".to_owned(),
            Box::new(Canon::Map(vec![
                ("after".to_owned(), Canon::Str(self.after.hex())),
                ("before".to_owned(), Canon::Str(self.before.hex())),
                ("epoch".to_owned(), Canon::Int(self.epoch)),
                (
                    "federationId".to_owned(),
                    Canon::Str(self.federation_id.hex()),
                ),
                (
                    "replicaSet".to_owned(),
                    Canon::Str(self.replica_set.hex()),
                ),
                (
                    "transition".to_owned(),
                    Canon::Str(self.transition.hex()),
                ),
            ])),
        );
        let artifact = Artifact {
            kind: "federation-proposal".to_owned(),
            body,
        };
        artifact.digest()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FederationFinalityCertificate {
    pub proposal: Digest,
    pub transition: Digest,
    pub state_digest: Digest,
    pub view: i64,
    pub seq: i64,
    pub commits: Vec<SignedCommit>,
    pub replica_set: Digest,
    pub epoch: i64,
    pub previous_state: Digest,
    pub federation_id: Digest,
}

impl FederationFinalityCertificate {
    pub fn from_artifact(a: &Artifact) -> Result<Self> {
        if a.kind != "certificate" {
            bail!("artifact is not a certificate");
        }
        Self::from_canon(&a.body)
    }

    pub fn from_canon(c: &Canon) -> Result<Self> {
        let m = c.expect_tag("federation-finality")?;
        let commits = m
            .field("commits")?
            .as_list()?
            .iter()
            .map(|row| {
                Ok(SignedCommit {
                    replica: row.field("replica")?.as_str()?.to_owned(),
                    seal: match row.field("seal")? {
                        Canon::Bytes(bs) => bs.clone(),
                        _ => bail!("seal"),
                    },
                })
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Self {
            proposal: Digest::parse(m.field("proposal")?.as_str()?)?,
            transition: Digest::parse(m.field("transition")?.as_str()?)?,
            state_digest: Digest::parse(m.field("state")?.as_str()?)?,
            view: m.field("view")?.as_int()?,
            seq: m.field("seq")?.as_int()?,
            commits,
            replica_set: Digest::parse(m.field("replicaSet")?.as_str()?)?,
            epoch: m.field("epoch")?.as_int()?,
            previous_state: Digest::parse(m.field("previousState")?.as_str()?)?,
            federation_id: Digest::parse(m.field("federationId")?.as_str()?)?,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FederationState {
    pub ledger: Digest,
    pub repository: Digest,
    pub applications: Digest,
    pub namespaces: Digest,
    pub trust_roots: Digest,
    pub gc_epoch: Digest,
}

impl FederationState {
    pub fn from_artifact(a: &Artifact) -> Result<Self> {
        if a.kind != "federation-state" {
            bail!("artifact is not a federation state");
        }
        let m = a.body.expect_tag("federation-state-v1")?;
        Ok(Self {
            ledger: Digest::parse(m.field("ledger")?.as_str()?)?,
            repository: Digest::parse(m.field("repository")?.as_str()?)?,
            applications: Digest::parse(m.field("applications")?.as_str()?)?,
            namespaces: Digest::parse(m.field("namespaces")?.as_str()?)?,
            trust_roots: Digest::parse(m.field("trustRoots")?.as_str()?)?,
            gc_epoch: Digest::parse(m.field("gcEpoch")?.as_str()?)?,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FederationTransition {
    pub before: Digest,
    pub transactions: Vec<Digest>,
    pub after: Digest,
    pub approvals: Vec<Digest>,
    pub finality: Option<Digest>,
}

impl FederationTransition {
    pub fn from_artifact(a: &Artifact) -> Result<Self> {
        if a.kind != "federation-transition" {
            bail!("artifact is not a federation transition");
        }
        let m = a.body.expect_tag("federation-transition-v1")?;
        let transactions = m
            .field("transactions")?
            .as_list()?
            .iter()
            .map(|x| Digest::parse(x.as_str()?))
            .collect::<Result<Vec<_>>>()?;
        let approvals = m
            .field("approvals")?
            .as_list()?
            .iter()
            .map(|x| Digest::parse(x.as_str()?))
            .collect::<Result<Vec<_>>>()?;
        let finality = match m.field("finality")? {
            Canon::Tag(tag, inner) if tag == "some" => Some(Digest::parse(inner.as_str()?)?),
            _ => None,
        };
        Ok(Self {
            before: Digest::parse(m.field("before")?.as_str()?)?,
            transactions,
            after: Digest::parse(m.field("after")?.as_str()?)?,
            approvals,
            finality,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReplicaSetManifest {
    pub authorities: Vec<(String, Vec<u8>)>,
    pub seals: Vec<(String, Vec<u8>)>,
    pub digest: Digest,
    pub replica_set_digest: Digest,
}

impl ReplicaSetManifest {
    pub fn from_artifact(a: &Artifact) -> Result<Self> {
        if a.kind != "certificate" {
            bail!("artifact is not a certificate");
        }
        let m = a.body.expect_tag("replica-set-manifest")?;
        let body = m.field("body")?;
        let reps = body
            .field("replicas")?
            .as_list()?
            .iter()
            .map(|row| {
                Ok((
                    row.field("id")?.as_str()?.to_owned(),
                    match row.field("publicKey")? {
                        Canon::Bytes(bs) => bs.clone(),
                        _ => bail!("publicKey"),
                    },
                ))
            })
            .collect::<Result<Vec<_>>>()?;
        let seals = m
            .field("seals")?
            .as_list()?
            .iter()
            .map(|row| {
                Ok((
                    row.field("id")?.as_str()?.to_owned(),
                    match row.field("seal")? {
                        Canon::Bytes(bs) => bs.clone(),
                        _ => bail!("seal"),
                    },
                ))
            })
            .collect::<Result<Vec<_>>>()?;

        let digest = a.digest();
        let replica_set_digest = Digest::of_bytes(&body.encode());
        Ok(Self {
            authorities: reps,
            seals,
            digest,
            replica_set_digest,
        })
    }

    pub fn authority_map(&self) -> std::collections::BTreeMap<String, Vec<u8>> {
        self.authorities
            .iter()
            .map(|(id, pk)| (id.clone(), pk.clone()))
            .collect()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypedKey {
    pub kind: String,
    pub value_hash: Digest,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Tx {
    PublishArtifact(TypedKey),
    RecordCertificate(Digest, String),
    Other,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SignedTx {
    pub tx: Tx,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Block {
    pub digest: Digest,
    pub txs: Vec<SignedTx>,
}

impl Block {
    pub fn from_artifact(a: &Artifact) -> Result<Self> {
        if a.kind != "block" {
            bail!("artifact is not a block");
        }
        let digest = a.digest();
        let c = &a.body;
        let block = c.field("block")?;
        let txs = block
            .field("txs")?
            .as_list()?
            .iter()
            .map(SignedTx::from_canon)
            .collect::<Result<Vec<_>>>()?;
        Ok(Self { digest, txs })
    }
}

impl SignedTx {
    fn from_canon(c: &Canon) -> Result<Self> {
        let tx = c.field("tx")?;
        let parsed = match tx {
            Canon::Tag(tag, inner) if tag == "publish-artifact" => {
                let value_hash = Digest::parse(inner.field("value")?.as_str()?)?;
                let kind = inner.field("kind")?.as_str()?.to_owned();
                Tx::PublishArtifact(TypedKey { kind, value_hash })
            }
            Canon::Tag(tag, inner) if tag == "record-certificate" => {
                let cert = Digest::parse(inner.field("cert")?.as_str()?)?;
                let method = inner.field("method")?.as_str()?.to_owned();
                Tx::RecordCertificate(cert, method)
            }
            _ => Tx::Other,
        };
        Ok(Self { tx: parsed })
    }
}

pub fn proposal_value_digest(proposal_digest: Digest) -> Digest {
    let v = Canon::Bytes(proposal_digest.to_ascii_hex_bytes());
    Digest::of_bytes(&v.encode())
}

pub fn signed_msg_payload_for_commit(
    view: i64,
    seq: i64,
    digest: Digest,
    from: &str,
    replica_set: Digest,
    chain_id: Digest,
) -> Vec<u8> {
    let msg = Canon::Tag(
        "commit".to_owned(),
        Box::new(Canon::Map(vec![
            ("digest".to_owned(), Canon::Str(digest.hex())),
            ("from".to_owned(), Canon::Str(from.to_owned())),
            ("seq".to_owned(), Canon::Int(seq)),
            ("view".to_owned(), Canon::Int(view)),
        ])),
    );
    let payload = Canon::Map(vec![
        ("chainId".to_owned(), Canon::Str(chain_id.hex())),
        ("domain".to_owned(), Canon::Str("cairn-bft-v1".to_owned())),
        ("msg".to_owned(), msg),
        ("replicaSet".to_owned(), Canon::Str(replica_set.hex())),
    ]);
    payload.encode()
}

pub fn parse_chain_file(text: &str) -> Result<Vec<Digest>> {
    let mut out = Vec::new();
    for line in text.lines() {
        let s = line.trim();
        if s.is_empty() {
            continue;
        }
        out.push(Digest::parse(s)?);
    }
    Ok(out)
}

pub fn quorum_size(n: usize) -> usize {
    ((2 * n) / 3) + 1
}

pub fn valid_replica_count(n: usize) -> bool {
    n == 1 || (n >= 4 && (n - 1) % 3 == 0)
}

pub fn as_i64_usize(v: i64, label: &str) -> Result<usize> {
    if v < 0 {
        bail!("{} must be non-negative", label);
    }
    usize::try_from(v).map_err(|_| anyhow!("{} out of range", label))
}
