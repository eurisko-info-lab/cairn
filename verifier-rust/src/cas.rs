use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{bail, Result};

use crate::digest::Digest;

#[derive(Clone, Debug)]
pub struct DiskCas {
    root: PathBuf,
}

impl DiskCas {
    pub fn new(root: impl AsRef<Path>) -> Self {
        Self {
            root: root.as_ref().to_path_buf(),
        }
    }

    pub fn path_of(&self, d: Digest) -> PathBuf {
        let hex = d.hex();
        self.root
            .join("objects")
            .join(&hex[0..2])
            .join(&hex[2..])
    }

    pub fn read_blob(&self, d: Digest) -> Result<Vec<u8>> {
        let p = self.path_of(d);
        if !p.exists() {
            bail!("blob {} not in CAS at {}", d.short(), self.root.display());
        }
        let bs = fs::read(&p)?;
        let actual = Digest::of_bytes(&bs);
        if actual != d {
            bail!("CAS corruption: blob {} hashes to {}", d.short(), actual.short());
        }
        Ok(bs)
    }

    pub fn read_text_file(&self, path: impl AsRef<Path>) -> Result<String> {
        let p = path.as_ref();
        let s = fs::read_to_string(p)?;
        Ok(s)
    }
}
