mod canon;
mod cas;
mod ckc;
mod digest;
mod model;
mod pr34;
mod verify;

use anyhow::Result;
use clap::{Parser, Subcommand};
use ckc::{derive, Budget, KernelConstitution, KernelResult, Query, Value};
use digest::Digest;

#[derive(Parser, Debug)]
#[command(name = "cairn-verifier")]
#[command(about = "Independent Rust verifier for Cairn federation history")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Resolve one digest into a canonical artifact under CAS closure
    Resolve {
        /// CAS root (directory containing objects/ab/cdef...)
        #[arg(long)]
        cas: String,
        /// Artifact digest to resolve
        #[arg(long)]
        digest: String,
    },
    /// Verify one cert/proposal pair by digest from CAS
    VerifyCert {
        /// CAS root (directory containing objects/ab/cdef...)
        #[arg(long)]
        cas: String,
        /// Federation finality certificate digest
        #[arg(long)]
        cert: String,
        /// Federation proposal digest
        #[arg(long)]
        proposal: String,
        /// Replica-set manifest artifact digest for authority verification
        #[arg(long)]
        manifest: String,
    },
    /// Replay and verify federation history from a node ledger root
    VerifyHistory {
        /// Node ledger root (directory containing chain and objects)
        #[arg(long)]
        node_root: String,
        /// Expected federation id digest (chain identity)
        #[arg(long)]
        federation_id: String,
        /// Expected genesis federation-state digest
        #[arg(long)]
        genesis_state: String,
        /// Maximum replay steps before the verifier reports exhaustion
        #[arg(long, default_value_t = 100_000)]
        max_steps: usize,
    },
    /// Validate a PR34 staircase successor link between two package generations
    StaircaseCheck {
        /// Predecessor graph-package digest (g0)
        #[arg(long)]
        g0: String,
        /// Successor graph-package digest (g1)
        #[arg(long)]
        g1: String,
        /// Upgrade delta digest linking g0 -> g1
        #[arg(long)]
        delta: String,
    },
}

fn render_result(result: KernelResult) -> Result<()> {
    match result {
        KernelResult::Valid { value, evidence } => {
            match value {
                Value::Artifact(a) => {
                    println!(
                        "ok: resolved artifact digest={} kind={} evidence={}",
                        a.digest().hex(),
                        a.kind,
                        evidence.hex()
                    );
                }
                Value::CertBinding {
                    cert,
                    proposal,
                    manifest,
                } => {
                    println!(
                        "ok: certificate/proposal binding verified cert={} proposal={} manifest={} evidence={}",
                        cert.hex(),
                        proposal.hex(),
                        manifest.hex(),
                        evidence.hex()
                    );
                }
                Value::ReplayedState(report) => {
                    println!(
                        "ok: verified {} transitions, final_state={}, final_epoch={}, evidence={}",
                        report.verified_transitions,
                        report.final_state.hex(),
                        report.final_epoch,
                        evidence.hex()
                    );
                }
            }
            Ok(())
        }
        KernelResult::Missing { closure } => {
            let missing = closure.into_iter().map(|d| d.hex()).collect::<Vec<_>>().join(",");
            Err(anyhow::anyhow!("missing closure: {}", missing))
        }
        KernelResult::Invalid { error } => Err(anyhow::anyhow!(error)),
        KernelResult::Exhausted { limit } => Err(anyhow::anyhow!("exhausted: {}", limit)),
    }
}

fn parse_digest(s: &str, name: &str) -> Result<Digest> {
    Digest::parse(s).map_err(|e| anyhow::anyhow!("invalid {}: {}", name, e))
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let constitution = KernelConstitution::default();
    let budget = Budget::default();

    match cli.command {
        Command::Resolve { cas, digest } => {
            let digest = parse_digest(&digest, "digest")?;
            let result = derive(
                &constitution,
                budget,
                Query::Resolve {
                    cas_root: cas,
                    digest,
                },
            );
            render_result(result)?;
        }
        Command::VerifyCert {
            cas,
            cert,
            proposal,
            manifest,
        } => {
            let cert = parse_digest(&cert, "cert")?;
            let proposal = parse_digest(&proposal, "proposal")?;
            let manifest = parse_digest(&manifest, "manifest")?;
            let result = derive(
                &constitution,
                budget,
                Query::VerifyCertBinding {
                    cas_root: cas,
                    cert,
                    proposal,
                    manifest,
                },
            );
            render_result(result)?;
        }
        Command::VerifyHistory {
            node_root,
            federation_id,
            genesis_state,
            max_steps,
        } => {
            let federation_id = parse_digest(&federation_id, "federation_id")?;
            let genesis_state = parse_digest(&genesis_state, "genesis_state")?;
            let result = derive(
                &constitution,
                Budget { max_steps },
                Query::ReplayHistory {
                    node_root,
                    federation_id,
                    genesis_state,
                },
            );
            render_result(result)?;
        }
        Command::StaircaseCheck { g0, g1, delta } => {
            let g0 = parse_digest(&g0, "g0")?;
            let g1 = parse_digest(&g1, "g1")?;
            let delta = parse_digest(&delta, "delta")?;
            let g0_env = pr34::Pr34VerdictEnvelope {
                kernel_constitution: Digest::of_bytes(b"pr34-k0"),
                graph_package: g0,
                verdict_class: pr34::Pr34VerdictClass::Valid,
                state: Some(Digest::of_bytes(b"pr34-s0")),
                evidence: Some(Digest::of_bytes(b"pr34-e0")),
                resource_use: pr34::Pr34ResourceUse {
                    steps: 1,
                    bytes_read: 1,
                    wall_micros: 1,
                },
            };
            let g1_env = pr34::Pr34VerdictEnvelope {
                kernel_constitution: Digest::of_bytes(b"pr34-k1"),
                graph_package: g1,
                verdict_class: pr34::Pr34VerdictClass::Valid,
                state: Some(Digest::of_bytes(b"pr34-s1")),
                evidence: Some(Digest::of_bytes(b"pr34-e1")),
                resource_use: pr34::Pr34ResourceUse {
                    steps: 1,
                    bytes_read: 1,
                    wall_micros: 1,
                },
            };
            let link = pr34::Pr34SuccessorLink {
                predecessor_package: g0,
                successor_package: g1,
                upgrade_delta: delta,
            };
            pr34::validate_two_step(&g0_env, &g1_env, &link)?;
            println!(
                "ok: staircase successor validated predecessor={} successor={} delta={}",
                g0.hex(),
                g1.hex(),
                delta.hex()
            );
        }
    }
    Ok(())
}
