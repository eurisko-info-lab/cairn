use std::collections::BTreeMap;

use anyhow::{anyhow, bail, Result};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Canon {
    Int(i64),
    Str(String),
    Bytes(Vec<u8>),
    List(Vec<Canon>),
    Map(Vec<(String, Canon)>),
    Tag(String, Box<Canon>),
}

impl Canon {
    pub fn decode(bs: &[u8]) -> Result<Canon> {
        decode_with_depth(bs, 256)
    }

    pub fn encode(&self) -> Vec<u8> {
        fn put_i32(out: &mut Vec<u8>, v: i32) {
            out.extend_from_slice(&v.to_be_bytes());
        }
        fn put_i64(out: &mut Vec<u8>, v: i64) {
            out.extend_from_slice(&v.to_be_bytes());
        }
        fn put_str(out: &mut Vec<u8>, s: &str) {
            let b = s.as_bytes();
            put_i32(out, b.len() as i32);
            out.extend_from_slice(b);
        }
        fn go(c: &Canon, out: &mut Vec<u8>) {
            match c {
                Canon::Int(v) => {
                    out.push(b'I');
                    put_i64(out, *v);
                }
                Canon::Str(v) => {
                    out.push(b'S');
                    put_str(out, v);
                }
                Canon::Bytes(v) => {
                    out.push(b'B');
                    put_i32(out, v.len() as i32);
                    out.extend_from_slice(v);
                }
                Canon::List(xs) => {
                    out.push(b'L');
                    put_i32(out, xs.len() as i32);
                    for x in xs {
                        go(x, out);
                    }
                }
                Canon::Map(es) => {
                    let mut sorted = es.clone();
                    sorted.sort_by(|a, b| a.0.as_bytes().cmp(b.0.as_bytes()));
                    out.push(b'M');
                    put_i32(out, sorted.len() as i32);
                    for (k, v) in &sorted {
                        put_str(out, k);
                        go(v, out);
                    }
                }
                Canon::Tag(t, v) => {
                    out.push(b'T');
                    put_str(out, t);
                    go(v, out);
                }
            }
        }
        let mut out = Vec::new();
        go(self, &mut out);
        out
    }

    pub fn as_str(&self) -> Result<&str> {
        match self {
            Canon::Str(s) => Ok(s),
            _ => bail!("expected string, got {:?}", self),
        }
    }

    pub fn as_int(&self) -> Result<i64> {
        match self {
            Canon::Int(v) => Ok(*v),
            _ => bail!("expected int, got {:?}", self),
        }
    }

    pub fn as_list(&self) -> Result<&[Canon]> {
        match self {
            Canon::List(xs) => Ok(xs),
            _ => bail!("expected list, got {:?}", self),
        }
    }

    pub fn as_map(&self) -> Result<BTreeMap<&str, &Canon>> {
        match self {
            Canon::Map(es) => {
                let mut m = BTreeMap::new();
                for (k, v) in es {
                    if m.insert(k.as_str(), v).is_some() {
                        bail!("duplicate map key: {}", k);
                    }
                }
                Ok(m)
            }
            _ => bail!("expected map, got {:?}", self),
        }
    }

    pub fn field<'a>(&'a self, key: &str) -> Result<&'a Canon> {
        let m = self.as_map()?;
        m.get(key)
            .copied()
            .ok_or_else(|| anyhow!("missing field '{}'", key))
    }

    pub fn expect_tag<'a>(&'a self, expected: &str) -> Result<&'a Canon> {
        match self {
            Canon::Tag(tag, value) if tag == expected => Ok(value),
            Canon::Tag(tag, _) => bail!("expected tag '{}', got '{}'", expected, tag),
            _ => bail!("expected tagged value '{}', got {:?}", expected, self),
        }
    }
}

fn decode_with_depth(bs: &[u8], max_depth: usize) -> Result<Canon> {
    let mut i = 0usize;

    fn fail(i: usize, msg: &str) -> anyhow::Error {
        anyhow!("canon decode at {}: {}", i, msg)
    }

    fn take_u8(bs: &[u8], i: &mut usize) -> Result<u8> {
        if *i >= bs.len() {
            bail!("eof");
        }
        let b = bs[*i];
        *i += 1;
        Ok(b)
    }

    fn take_i32(bs: &[u8], i: &mut usize) -> Result<i32> {
        if *i + 4 > bs.len() {
            bail!("eof int");
        }
        let mut arr = [0u8; 4];
        arr.copy_from_slice(&bs[*i..*i + 4]);
        *i += 4;
        Ok(i32::from_be_bytes(arr))
    }

    fn take_i64(bs: &[u8], i: &mut usize) -> Result<i64> {
        if *i + 8 > bs.len() {
            bail!("eof long");
        }
        let mut arr = [0u8; 8];
        arr.copy_from_slice(&bs[*i..*i + 8]);
        *i += 8;
        Ok(i64::from_be_bytes(arr))
    }

    fn take_count(bs: &[u8], i: &mut usize, label: &str) -> Result<usize> {
        let n = take_i32(bs, i)?;
        if n < 0 {
            bail!("negative {} count", label);
        }
        let n = n as usize;
        if n > bs.len().saturating_sub(*i) {
            bail!("{} count exceeds remaining bytes", label);
        }
        Ok(n)
    }

    fn take_str(bs: &[u8], i: &mut usize) -> Result<String> {
        let n = take_count(bs, i, "string")?;
        if *i + n > bs.len() {
            bail!("eof string");
        }
        let s = std::str::from_utf8(&bs[*i..*i + n]).map_err(|_| anyhow!("invalid UTF-8 in string"))?;
        *i += n;
        Ok(s.to_owned())
    }

    fn go(bs: &[u8], i: &mut usize, depth: usize, max_depth: usize) -> Result<Canon> {
        if depth > max_depth {
            bail!("nesting depth exceeds {}", max_depth);
        }
        let t = take_u8(bs, i)?;
        match t {
            b'I' => Ok(Canon::Int(take_i64(bs, i)?)),
            b'S' => Ok(Canon::Str(take_str(bs, i)?)),
            b'B' => {
                let n = take_count(bs, i, "bytes")?;
                if *i + n > bs.len() {
                    bail!("eof bytes");
                }
                let v = bs[*i..*i + n].to_vec();
                *i += n;
                Ok(Canon::Bytes(v))
            }
            b'L' => {
                let n = take_count(bs, i, "list")?;
                let mut xs = Vec::with_capacity(n);
                for _ in 0..n {
                    xs.push(go(bs, i, depth + 1, max_depth)?);
                }
                Ok(Canon::List(xs))
            }
            b'M' => {
                let n = take_count(bs, i, "map")?;
                let mut es = Vec::with_capacity(n);
                for _ in 0..n {
                    let k = take_str(bs, i)?;
                    let v = go(bs, i, depth + 1, max_depth)?;
                    es.push((k, v));
                }
                for idx in 1..es.len() {
                    if es[idx - 1].0.as_bytes() >= es[idx].0.as_bytes() {
                        bail!("map entries not in canonical sorted order (or duplicate key) at entry {}", idx);
                    }
                }
                Ok(Canon::Map(es))
            }
            b'T' => {
                let tag = take_str(bs, i)?;
                let value = go(bs, i, depth + 1, max_depth)?;
                Ok(Canon::Tag(tag, Box::new(value)))
            }
            other => bail!("unknown tag byte {}", other),
        }
    }

    let c = go(bs, &mut i, 0, max_depth).map_err(|e| fail(i, &e.to_string()))?;
    if i != bs.len() {
        bail!("trailing bytes after canon value ({} of {})", i, bs.len());
    }
    Ok(c)
}
