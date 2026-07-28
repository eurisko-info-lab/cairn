mod canon;
mod cas;
mod digest;
mod model;
mod verify;

use anyhow::Result;
use clap::{Parser, Subcommand};
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
    },
}

fn parse_digest(s: &str, name: &str) -> Result<Digest> {
    Digest::parse(s).map_err(|e| anyhow::anyhow!("invalid {}: {}", name, e))
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Command::VerifyCert {
            cas,
            cert,
            proposal,
            manifest,
        } => {
            let cert = parse_digest(&cert, "cert")?;
            let proposal = parse_digest(&proposal, "proposal")?;
            let manifest = parse_digest(&manifest, "manifest")?;
            verify::verify_cert_command(&cas, cert, proposal, manifest)?;
            println!("ok: certificate/proposal binding verified");
        }
        Command::VerifyHistory {
            node_root,
            federation_id,
            genesis_state,
        } => {
            let federation_id = parse_digest(&federation_id, "federation_id")?;
            let genesis_state = parse_digest(&genesis_state, "genesis_state")?;
            let report = verify::verify_history_command(&node_root, federation_id, genesis_state)?;
            println!(
                "ok: verified {} transitions, final_state={}, final_epoch={}",
                report.verified_transitions,
                report.final_state.hex(),
                report.final_epoch
            );
        }
    }
    Ok(())
}
