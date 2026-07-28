use anyhow::{anyhow, Result};
use sha2::{Digest as ShaDigestTrait, Sha256};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Digest {
    pub bytes: [u8; 32],
}

impl Digest {
    pub fn parse(s: &str) -> Result<Self> {
        if s.len() != 64 {
            return Err(anyhow!("digest must be 64 hex chars, got {}", s.len()));
        }
        let raw = hex::decode(s).map_err(|e| anyhow!("invalid digest hex: {}", e))?;
        if raw.len() != 32 {
            return Err(anyhow!("digest must decode to 32 bytes"));
        }
        let mut bytes = [0u8; 32];
        bytes.copy_from_slice(&raw);
        Ok(Self { bytes })
    }

    pub fn of_bytes(bs: &[u8]) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(bs);
        let out = hasher.finalize();
        let mut bytes = [0u8; 32];
        bytes.copy_from_slice(&out);
        Self { bytes }
    }

    pub fn short(&self) -> String {
        self.hex().chars().take(12).collect()
    }

    pub fn to_ascii_hex_bytes(&self) -> Vec<u8> {
        self.hex().into_bytes()
    }

    pub fn hex(&self) -> String {
        hex::encode(self.bytes)
    }
}
