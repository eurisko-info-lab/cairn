import Std

namespace VerifierLean

abbrev Digest := String

inductive ArtifactKind where
  | proposal
  | certificate
  | replicaManifest
  | other (name : String)
  deriving Repr, BEq, DecidableEq

structure Artifact where
  digest : Digest
  kind : ArtifactKind
  deriving Repr, BEq, DecidableEq

structure CertificateProjection where
  cert : Digest
  proposal : Digest
  manifest : Digest
  federationId : Digest
  epoch : Nat
  deriving Repr, BEq, DecidableEq

structure Transition where
  before : Digest
  after : Digest
  cert : Digest
  proposal : Digest
  manifest : Digest
  federationId : Digest
  epoch : Nat
  deriving Repr, BEq, DecidableEq

structure HistoryReport where
  verifiedTransitions : Nat
  finalState : Digest
  finalEpoch : Nat
  deriving Repr, BEq, DecidableEq

structure KernelConstitution where
  kernelId : String := "ckc-v0"
  deriving Repr, BEq, DecidableEq

structure Budget where
  maxSteps : Nat := 100000
  deriving Repr, BEq, DecidableEq

inductive Query where
  | resolve (casRoot : String) (digest : Digest)
  | verifyCertBinding (casRoot : String) (cert : Digest) (proposal : Digest) (manifest : Digest)
  | replayHistory (nodeRoot : String) (federationId : Digest) (genesisState : Digest)
  deriving Repr, BEq, DecidableEq

inductive Value where
  | artifact (a : Artifact)
  | certBinding (cert : Digest) (proposal : Digest) (manifest : Digest)
  | replayedState (report : HistoryReport)
  deriving Repr, BEq, DecidableEq

inductive KernelResult where
  | valid (value : Value) (evidence : Digest)
  | invalid (error : String)
  | missing (closure : List Digest)
  | exhausted (limit : String)
  deriving Repr, BEq, DecidableEq

namespace CKC

/-- Protocol identity is defined by digest equality. -/
def ProtocolIdentity (a b : Digest) : Prop :=
  a = b

/-- Cryptographic collision resistance remains an explicit external assumption. -/
def CollisionResistanceAssumption : Prop :=
  True

structure GenericMachineRef where
  digest : Digest
  deriving Repr, BEq, DecidableEq

structure LanguageRef where
  digest : Digest
  machine : GenericMachineRef
  deriving Repr, BEq, DecidableEq

structure AcceptanceConstitutionRef where
  digest : Digest
  deriving Repr, BEq, DecidableEq

structure FreeChangeWitness where
  source : LanguageRef
  result : LanguageRef
  kernelId : String
  deriving Repr, BEq, DecidableEq

/-- Free-change is indexed by the active kernel constitution. -/
def FreeChange (K : KernelConstitution) (L : LanguageRef) : FreeChangeWitness :=
  let d := s!"delta({K.kernelId},{L.digest})"
  let out : LanguageRef := { digest := d, machine := L.machine }
  { source := L, result := out, kernelId := K.kernelId }

theorem freeChange_preserves_machine (K : KernelConstitution) (L : LanguageRef) :
    (FreeChange K L).result.machine = L.machine := by
  rfl

inductive Judgment where
  | resolve (digest : Digest)
  | languageAt (language : LanguageRef)
  | freeChangeLanguage (language : LanguageRef)
  | applyChange (language : LanguageRef) (stateDigest : Digest) (changeDigest : Digest)
  | accept
      (rho : AcceptanceConstitutionRef)
      (authority : Digest)
      (claim : Digest)
      (proof : Digest)
  | replayHistory (federationId : Digest) (genesisState : Digest)
  deriving Repr, BEq, DecidableEq

inductive Verdict (α : Type) where
  | valid (value : α) (evidence : Digest)
  | invalid (error : String)
  | missing (closure : List Digest)
  | exhausted (limit : String)
  deriving Repr, BEq, DecidableEq

end CKC

structure Context where
  artifacts : Std.HashMap Digest Artifact
  certs : Std.HashMap Digest CertificateProjection
  history : List Transition

namespace Context

def empty : Context where
  artifacts := {}
  certs := {}
  history := []

end Context

def isHexChar (c : Char) : Bool :=
  c.isDigit || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')

def extractHex64Tokens (text : String) : List Digest :=
  let flush (acc : List Digest) (buf : String) : List Digest :=
    if buf.length == 64 then
      acc.concat buf
    else
      acc
  let (tokens, tail) :=
    text.toList.foldl
      (fun st ch =>
        let (acc, buf) := st
        if isHexChar ch then
          (acc, buf.push ch)
        else
          (flush acc buf, ""))
      ([], "")
  (flush tokens tail).eraseDups

def isDigest (s : String) : Bool :=
  s.length == 64 && s.toList.all isHexChar

def requireDigest (label : String) (s : String) : Except String Digest :=
  if isDigest s then
    .ok s
  else
    .error s!"{label} is not a 64-hex digest: {s}"

def casPath (root : System.FilePath) (digest : Digest) : Except String System.FilePath := do
  if digest.length != 64 then
    throw s!"invalid digest length for '{digest}'"
  let dir := (digest.take 2).toString
  let file := (digest.drop 2).toString
  pure (root / "objects" / dir / file)

def checkDigestExists (root : System.FilePath) (digest : Digest) : IO (Except String Bool) := do
  match casPath root digest with
  | .error e =>
      pure (.error e)
  | .ok p => do
      let present ← p.pathExists
      pure (.ok present)

def readChainDigests (nodeRoot : String) : IO (Except String (List Digest)) := do
  let chainPath := System.FilePath.mk nodeRoot / "chain"
  let present ← chainPath.pathExists
  if !present then
    pure (.error s!"missing chain file: {chainPath}")
  else
    let content ← IO.FS.readFile chainPath
    let ds := extractHex64Tokens content
    if ds.isEmpty then
      pure (.error "empty chain")
    else
      pure (.ok ds)

inductive Canon where
  | int (value : Int)
  | str (value : String)
  | bytes (value : ByteArray)
  | list (value : List Canon)
  | map (value : List (String × Canon))
  | tag (name : String) (value : Canon)
  deriving BEq

def canonKind : Canon → String
  | .int _ => "int"
  | .str _ => "str"
  | .bytes _ => "bytes"
  | .list _ => "list"
  | .map _ => "map"
  | .tag _ _ => "tag"

def readU8 (bs : ByteArray) (i : Nat) : Except String (UInt8 × Nat) :=
  if _h : i < bs.size then
    .ok (bs.get! i, i + 1)
  else
    .error s!"canon decode at {i}: eof"

def readN (bs : ByteArray) (i n : Nat) : Except String (ByteArray × Nat) :=
  if i + n <= bs.size then
    .ok (bs.extract i (i + n), i + n)
  else
    .error s!"canon decode at {i}: eof"

def readI32BE (bs : ByteArray) (i : Nat) : Except String (Int × Nat) := do
  let (chunk, j) <- readN bs i 4
  let u : Nat :=
    (((chunk.get! 0).toNat <<< 24) ||| ((chunk.get! 1).toNat <<< 16) |||
      ((chunk.get! 2).toNat <<< 8) ||| (chunk.get! 3).toNat)
  let signed : Int :=
    if u <= 2147483647 then
      Int.ofNat u
    else
      Int.ofNat u - Int.ofNat 4294967296
  pure (signed, j)

def readI64BE (bs : ByteArray) (i : Nat) : Except String (Int × Nat) := do
  let (chunk, j) <- readN bs i 8
  let u : Nat :=
    (((chunk.get! 0).toNat <<< 56) ||| ((chunk.get! 1).toNat <<< 48) |||
      ((chunk.get! 2).toNat <<< 40) ||| ((chunk.get! 3).toNat <<< 32) |||
      ((chunk.get! 4).toNat <<< 24) ||| ((chunk.get! 5).toNat <<< 16) |||
      ((chunk.get! 6).toNat <<< 8) ||| (chunk.get! 7).toNat)
  let signed : Int :=
    if u <= 9223372036854775807 then
      Int.ofNat u
    else
      Int.ofNat u - Int.ofNat 18446744073709551616
  pure (signed, j)

def readCount (bs : ByteArray) (i : Nat) (label : String) : Except String (Nat × Nat) := do
  let (n, j) <- readI32BE bs i
  if n < 0 then
    .error s!"canon decode at {i}: negative {label} count"
  else
    pure (Int.toNat n, j)

def readUtf8 (bs : ByteArray) (i : Nat) : Except String (String × Nat) := do
  let (n, j) <- readCount bs i "string"
  let (chunk, k) <- readN bs j n
  match String.fromUTF8? chunk with
  | some s => pure (s, k)
  | none => .error s!"canon decode at {j}: invalid UTF-8 in string"

partial def decodeCanonAt (bs : ByteArray) (i depth : Nat) : Except String (Canon × Nat) := do
  if depth > 256 then
    .error s!"canon decode at {i}: nesting depth exceeds 256"
  let (tagByte, j) <- readU8 bs i
  match tagByte.toNat with
  | 73 =>
      let (v, k) <- readI64BE bs j
      pure (.int v, k)
  | 83 =>
      let (s, k) <- readUtf8 bs j
      pure (.str s, k)
  | 66 =>
      let (n, k0) <- readCount bs j "bytes"
      let (b, k) <- readN bs k0 n
      pure (.bytes b, k)
  | 76 =>
      let (n, k0) <- readCount bs j "list"
      let rec loopList (remaining : Nat) (k : Nat) (acc : List Canon) : Except String (List Canon × Nat) := do
        if remaining == 0 then
          pure (acc.reverse, k)
        else
          let (x, k2) <- decodeCanonAt bs k (depth + 1)
          loopList (remaining - 1) k2 (x :: acc)
      let (xs, k) <- loopList n k0 []
      pure (.list xs, k)
  | 77 =>
      let (n, k0) <- readCount bs j "map"
      let rec loopMap (remaining : Nat) (k : Nat) (prev : Option String)
          (acc : List (String × Canon)) : Except String (List (String × Canon) × Nat) := do
        if remaining == 0 then
          pure (acc.reverse, k)
        else
          let (key, k1) <- readUtf8 bs k
          match prev with
          | some p =>
              if !(p < key) then
                .error s!"canon decode at {k}: map entries not in canonical sorted order"
              else
                pure ()
          | none => pure ()
          let (value, k2) <- decodeCanonAt bs k1 (depth + 1)
          loopMap (remaining - 1) k2 (some key) ((key, value) :: acc)
      let (es, k) <- loopMap n k0 none []
      pure (.map es, k)
  | 84 =>
      let (name, k1) <- readUtf8 bs j
      let (value, k2) <- decodeCanonAt bs k1 (depth + 1)
      pure (.tag name value, k2)
  | other =>
      .error s!"canon decode at {i}: unknown tag byte {other}"

def decodeCanon (bs : ByteArray) : Except String Canon := do
  let (c, i) <- decodeCanonAt bs 0 0
  if i != bs.size then
    .error s!"canon decode at {i}: trailing bytes after canon value"
  else
    pure c

def canonAsMap (c : Canon) : Except String (List (String × Canon)) :=
  match c with
  | .map es => .ok es
  | _ => .error s!"expected map, got {canonKind c}"

def canonField (c : Canon) (key : String) : Except String Canon := do
  let es <- canonAsMap c
  match es.find? (fun (k, _) => k == key) with
  | some (_, v) => .ok v
  | none => .error s!"missing field '{key}'"

def canonAsStr (c : Canon) : Except String String :=
  match c with
  | .str s => .ok s
  | _ => .error s!"expected string, got {canonKind c}"

def canonAsInt (c : Canon) : Except String Int :=
  match c with
  | .int n => .ok n
  | _ => .error s!"expected int, got {canonKind c}"

def canonExpectTag (c : Canon) (name : String) : Except String Canon :=
  match c with
  | .tag actual value =>
      if actual == name then
        .ok value
      else
        .error s!"expected tag '{name}', got '{actual}'"
  | _ => .error s!"expected tagged value '{name}', got {canonKind c}"

structure ParsedArtifact where
  kind : String
  body : Canon

def decodeArtifact (bs : ByteArray) : Except String ParsedArtifact := do
  let root <- decodeCanon bs
  let kind <- canonAsStr (← canonField root "kind")
  let body <- canonField root "body"
  pure { kind := kind, body := body }

def readArtifactFromCas (root : System.FilePath) (digest : Digest) : IO (Except String ParsedArtifact) := do
  match casPath root digest with
  | .error e =>
      pure (.error e)
  | .ok p =>
      let present ← p.pathExists
      if !present then
        pure (.error s!"artifact not in CAS: {digest}")
      else
        let bytes ← IO.FS.readBinFile p
        pure (decodeArtifact bytes)

structure ProposalView where
  federationId : Digest
  transition : Digest
  before : Digest
  after : Digest
  epoch : Int
  replicaSet : Digest
  deriving Repr

def parseProposalView (a : ParsedArtifact) : Except String ProposalView := do
  if a.kind != "federation-proposal" then
    .error "artifact is not a federation proposal"
  let body <- canonExpectTag a.body "federation-proposal-v1"
  let federationId <- requireDigest "proposal.federationId" (← canonAsStr (← canonField body "federationId"))
  let transition <- requireDigest "proposal.transition" (← canonAsStr (← canonField body "transition"))
  let before <- requireDigest "proposal.before" (← canonAsStr (← canonField body "before"))
  let after <- requireDigest "proposal.after" (← canonAsStr (← canonField body "after"))
  let epoch <- canonAsInt (← canonField body "epoch")
  let replicaSet <- requireDigest "proposal.replicaSet" (← canonAsStr (← canonField body "replicaSet"))
  pure {
    federationId := federationId
    transition := transition
    before := before
    after := after
    epoch := epoch
    replicaSet := replicaSet
  }

structure CertView where
  proposal : Digest
  transition : Digest
  state : Digest
  view : Int
  seq : Int
  previousState : Digest
  federationId : Digest
  epoch : Int
  replicaSet : Digest
  commits : List (String × ByteArray)

def canonAsBytes (c : Canon) : Except String ByteArray :=
  match c with
  | .bytes bs => .ok bs
  | _ => .error s!"expected bytes, got {canonKind c}"

def parseCertView (a : ParsedArtifact) : Except String CertView := do
  if a.kind != "certificate" then
    .error "artifact is not a certificate"
  let body <- canonExpectTag a.body "federation-finality"
  let proposal <- requireDigest "certificate.proposal" (← canonAsStr (← canonField body "proposal"))
  let transition <- requireDigest "certificate.transition" (← canonAsStr (← canonField body "transition"))
  let state <- requireDigest "certificate.state" (← canonAsStr (← canonField body "state"))
  let view <- canonAsInt (← canonField body "view")
  let seq <- canonAsInt (← canonField body "seq")
  let previousState <- requireDigest "certificate.previousState" (← canonAsStr (← canonField body "previousState"))
  let federationId <- requireDigest "certificate.federationId" (← canonAsStr (← canonField body "federationId"))
  let epoch <- canonAsInt (← canonField body "epoch")
  let replicaSet <- requireDigest "certificate.replicaSet" (← canonAsStr (← canonField body "replicaSet"))
  let commitsCanon <- canonField body "commits"
  let commits <-
    match commitsCanon with
    | .list xs =>
        xs.mapM (fun row => do
          let replica <- canonAsStr (← canonField row "replica")
          let sealBytes <- canonAsBytes (← canonField row "seal")
          pure (replica, sealBytes))
    | _ => .error "certificate.commits must be list"
  pure {
    proposal := proposal
    transition := transition
    state := state
    view := view
    seq := seq
    previousState := previousState
    federationId := federationId
    epoch := epoch
    replicaSet := replicaSet
    commits := commits
  }

structure ManifestView where
  authorities : List (String × ByteArray)
  seals : List (String × ByteArray)
  body : Canon

def parseManifestView (a : ParsedArtifact) : Except String ManifestView := do
  if a.kind != "certificate" then
    .error "manifest artifact is not a certificate"
  let outer <- canonExpectTag a.body "replica-set-manifest"
  let body <- canonField outer "body"
  let replicasCanon <- canonField body "replicas"
  let authorities <-
    match replicasCanon with
    | .list xs =>
        xs.mapM (fun row => do
          let id <- canonAsStr (← canonField row "id")
          let publicKey <- canonAsBytes (← canonField row "publicKey")
          pure (id, publicKey))
    | _ => .error "replica-set-manifest.body.replicas must be list"
  let sealsCanon <- canonField outer "seals"
  let seals <-
    match sealsCanon with
    | .list xs =>
        xs.mapM (fun row => do
          let id <- canonAsStr (← canonField row "id")
          let sealBytes <- canonAsBytes (← canonField row "seal")
          pure (id, sealBytes))
    | _ => .error "replica-set-manifest.seals must be list"
  pure { authorities := authorities, seals := seals, body := body }

def validReplicaCount (n : Nat) : Bool :=
  n == 1 || (n >= 4 && (n - 1) % 3 == 0)

def quorumSize (n : Nat) : Nat :=
  ((2 * n) / 3) + 1

def verifyManifestCoverage (manifest : ManifestView) : Except String Unit := do
  let authIds := manifest.authorities.map (fun x => x.fst)
  let sealIds := manifest.seals.map (fun x => x.fst)
  let authDistinct := authIds.eraseDups
  let sealDistinct := sealIds.eraseDups
  if authDistinct.isEmpty then
    .error "replica-set: empty"
  else if authDistinct.length != authIds.length then
    .error "replica-set: duplicate authority ids"
  else if sealDistinct.length != sealIds.length then
    .error "replica-set: duplicate seal ids"
  else
    let missingAuth := authDistinct.filter (fun id => !(sealDistinct.contains id))
    let extraSeals := sealDistinct.filter (fun id => !(authDistinct.contains id))
    if !missingAuth.isEmpty || !extraSeals.isEmpty then
      .error "replica-set: seal coverage incomplete"
    else
      pure ()

def findAuthorityPk (manifest : ManifestView) (id : String) : Option ByteArray :=
  match manifest.authorities.find? (fun x => x.fst == id) with
  | some (_, pk) => some pk
  | none => none

def verifyCertQuorum (cert : CertView) (manifest : ManifestView) : Except String Unit := do
  let n := manifest.authorities.length
  if !validReplicaCount n then
    .error s!"federation finality: n={n} is not a valid 3f+1 size"
  else if cert.seq != cert.epoch then
    .error s!"federation finality: certificate sequence {cert.seq} does not equal epoch {cert.epoch}"
  else
    let commitIds := cert.commits.map (fun x => x.fst)
    let distinct := commitIds.eraseDups
    let authIds := manifest.authorities.map (fun x => x.fst)
    if distinct.length != commitIds.length then
      .error "federation finality: duplicate replica commits"
    else if distinct.any (fun id => !(authIds.contains id)) then
      .error "federation finality: unknown replica in commits"
    else if distinct.length < quorumSize n then
      .error s!"federation finality: {distinct.length} distinct commits < quorum {quorumSize n}"
    else
      pure ()

def appendBytes (acc extra : ByteArray) : ByteArray :=
  extra.foldl (fun out b => out.push b) acc

def putI32 (out : ByteArray) (v : Int) : ByteArray :=
  let u : Nat :=
    if v < 0 then
      Int.toNat (v + Int.ofNat 4294967296)
    else
      Int.toNat v
  out
    |>.push (UInt8.ofNat ((u >>> 24) &&& 0xFF))
    |>.push (UInt8.ofNat ((u >>> 16) &&& 0xFF))
    |>.push (UInt8.ofNat ((u >>> 8) &&& 0xFF))
    |>.push (UInt8.ofNat (u &&& 0xFF))

def putI64 (out : ByteArray) (v : Int) : ByteArray :=
  let u : Nat :=
    if v < 0 then
      Int.toNat (v + Int.ofNat 18446744073709551616)
    else
      Int.toNat v
  out
    |>.push (UInt8.ofNat ((u >>> 56) &&& 0xFF))
    |>.push (UInt8.ofNat ((u >>> 48) &&& 0xFF))
    |>.push (UInt8.ofNat ((u >>> 40) &&& 0xFF))
    |>.push (UInt8.ofNat ((u >>> 32) &&& 0xFF))
    |>.push (UInt8.ofNat ((u >>> 24) &&& 0xFF))
    |>.push (UInt8.ofNat ((u >>> 16) &&& 0xFF))
    |>.push (UInt8.ofNat ((u >>> 8) &&& 0xFF))
    |>.push (UInt8.ofNat (u &&& 0xFF))

def putString (out : ByteArray) (s : String) : ByteArray :=
  let b := s.toUTF8
  let out := putI32 out (Int.ofNat b.size)
  appendBytes out b

mutual
  partial def encodeCanon : Canon → ByteArray
    | .int v =>
        putI64 (ByteArray.empty.push (UInt8.ofNat 73)) v
    | .str s =>
        putString (ByteArray.empty.push (UInt8.ofNat 83)) s
    | .bytes bs =>
        let out := putI32 (ByteArray.empty.push (UInt8.ofNat 66)) (Int.ofNat bs.size)
        appendBytes out bs
    | .list xs =>
        let out := putI32 (ByteArray.empty.push (UInt8.ofNat 76)) (Int.ofNat xs.length)
        appendBytes out (encodeCanonList xs)
    | .map kvs =>
        let sorted := kvs.mergeSort (fun a b => a.fst < b.fst)
        let out := putI32 (ByteArray.empty.push (UInt8.ofNat 77)) (Int.ofNat sorted.length)
        appendBytes out (encodeMapEntries sorted)
    | .tag tag value =>
        let out := putString (ByteArray.empty.push (UInt8.ofNat 84)) tag
        appendBytes out (encodeCanon value)

  partial def encodeCanonList : List Canon → ByteArray
    | [] => ByteArray.empty
    | x :: xs =>
        appendBytes (encodeCanon x) (encodeCanonList xs)

  partial def encodeMapEntries : List (String × Canon) → ByteArray
    | [] => ByteArray.empty
    | (k, v) :: rest =>
        let head := appendBytes (putString ByteArray.empty k) (encodeCanon v)
        appendBytes head (encodeMapEntries rest)
end

def hashBytes64 (bs : ByteArray) : UInt64 :=
  let prime : UInt64 := 1099511628211
  let offset : UInt64 := 14695981039346656037
  bs.foldl (fun h b => (h ^^^ UInt64.ofNat b.toNat) * prime) offset

def tmpPathFor (pfx : String) (payload : ByteArray) (ext : String) : System.FilePath :=
  System.FilePath.mk "/tmp" / s!"{pfx}-{payload.size}-{toString (hashBytes64 payload)}.{ext}"

def sha256HexOfBytes (bytes : ByteArray) : IO (Except String Digest) := do
  let tmpPath := tmpPathFor "cairn-lean-hash" bytes "bin"
  try
    IO.FS.writeBinFile tmpPath bytes
    let out ← IO.Process.output {
      cmd := "sha256sum"
      args := #[tmpPath.toString]
      stdin := .null
      stderr := .piped
      stdout := .piped
    }
    if out.exitCode != 0 then
      pure (.error s!"sha256sum failed: {(out.stderr.trimAscii).toString}")
    else
      let token := ((out.stdout.trimAscii).toString).splitOn " " |>.headD ""
      if isDigest token then
        pure (.ok token)
      else
        pure (.error s!"sha256sum output did not contain digest: {(out.stdout.trimAscii).toString}")
  catch e =>
    pure (.error s!"sha256sum invocation failed: {e.toString}")
  finally
    try
      IO.FS.removeFile tmpPath
    catch _ =>
      pure ()

def verifyManifestDigestBindingIO (cert : CertView) (manifest : ManifestView) : IO (Except String Unit) := do
  let bodyBytes := encodeCanon manifest.body
  match ← sha256HexOfBytes bodyBytes with
  | .error e =>
      pure (.error e)
  | .ok computed =>
      if cert.replicaSet == computed then
        pure (.ok ())
      else
        pure (.error s!"federation finality: replicaSet {cert.replicaSet} != expected {computed}")

def natsToBytes (xs : List Nat) : ByteArray :=
  xs.foldl (fun acc n => acc.push (UInt8.ofNat n)) ByteArray.empty

def ed25519SpkiPrefix : ByteArray :=
  natsToBytes [0x30, 0x2a, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x70, 0x03, 0x21, 0x00]

def verifyEd25519WithOpenSSL (publicKey32 payload signature64 : ByteArray) : IO (Except String Bool) := do
  if publicKey32.size != 32 || signature64.size != 64 then
    pure (.ok false)
  else
    let keyDer := appendBytes ed25519SpkiPrefix publicKey32
    let keyPath := tmpPathFor "cairn-lean-ed25519-key" keyDer "pub.der"
    let msgPath := tmpPathFor "cairn-lean-ed25519-msg" payload "msg.bin"
    let sigPath := tmpPathFor "cairn-lean-ed25519-sig" signature64 "sig.bin"
    try
      IO.FS.writeBinFile keyPath keyDer
      IO.FS.writeBinFile msgPath payload
      IO.FS.writeBinFile sigPath signature64
      let out ← IO.Process.output {
        cmd := "openssl"
        args := #[
          "pkeyutl", "-verify", "-pubin", "-inkey", keyPath.toString, "-keyform", "DER",
          "-rawin", "-in", msgPath.toString, "-sigfile", sigPath.toString
        ]
        stdin := .null
        stderr := .piped
        stdout := .piped
      }
      if out.exitCode == 0 then
        pure (.ok true)
      else
        pure (.ok false)
    catch e =>
      pure (.error s!"openssl invocation failed: {e.toString}")
    finally
      try
        IO.FS.removeFile keyPath
      catch _ =>
        pure ()
      try
        IO.FS.removeFile msgPath
      catch _ =>
        pure ()
      try
        IO.FS.removeFile sigPath
      catch _ =>
        pure ()

def proposalValueDigestBytes (proposalDigest : Digest) : ByteArray :=
  encodeCanon (.bytes proposalDigest.toUTF8)

def buildSignedCommitPayload (cert : CertView) (replica : String) (valueDigest : Digest) : ByteArray :=
  let msg : Canon :=
    .tag "commit" (.map [
      ("digest", .str valueDigest),
      ("from", .str replica),
      ("seq", .int cert.seq),
      ("view", .int cert.view)
    ])
  let payload : Canon :=
    .map [
      ("chainId", .str cert.federationId),
      ("domain", .str "cairn-bft-v1"),
      ("msg", msg),
      ("replicaSet", .str cert.replicaSet)
    ]
  encodeCanon payload

def verifyManifestSealsIO (manifest : ManifestView) : IO (Except String Unit) := do
  match verifyManifestCoverage manifest with
  | .error e =>
      pure (.error e)
  | .ok _ =>
      let bodyBytes := encodeCanon manifest.body
      for (id, sealBytes) in manifest.seals do
        match findAuthorityPk manifest id with
        | none =>
            return .error s!"replica-set: seal for unknown id '{id}'"
        | some pk =>
            match ← verifyEd25519WithOpenSSL pk bodyBytes sealBytes with
            | .error e =>
                return .error e
            | .ok false =>
                return .error s!"replica-set: bad seal from '{id}'"
            | .ok true =>
                pure ()
      pure (.ok ())

def verifyCertSignaturesIO (cert : CertView) (manifest : ManifestView) : IO (Except String Unit) := do
  match verifyCertQuorum cert manifest with
  | .error e =>
      pure (.error e)
  | .ok _ =>
      match ← sha256HexOfBytes (proposalValueDigestBytes cert.proposal) with
      | .error e =>
          pure (.error e)
      | .ok valueDigest =>
          for (replica, sealBytes) in cert.commits do
            match findAuthorityPk manifest replica with
            | none =>
                return .error s!"unknown replica {replica}"
            | some pk =>
                let payload := buildSignedCommitPayload cert replica valueDigest
                match ← verifyEd25519WithOpenSSL pk payload sealBytes with
                | .error e =>
                    return .error e
                | .ok false =>
                    return .error s!"bad bft seal from {replica}"
                | .ok true =>
                    pure ()
          pure (.ok ())

def checkCommandAvailableIO (cmd : String) (args : Array String := #[]) : IO (Except String Unit) := do
  try
    let out ← IO.Process.output {
      cmd := cmd
      args := args
      stdin := .null
      stderr := .piped
      stdout := .piped
    }
    if out.exitCode == 0 then
      pure (.ok ())
    else
      pure (.error s!"runtime dependency '{cmd}' is unavailable (exit code {out.exitCode})")
  catch e =>
    pure (.error s!"runtime dependency '{cmd}' is unavailable: {e.toString}")

def verifyRuntimeDependenciesIO : IO (Except String Unit) := do
  match ← checkCommandAvailableIO "sha256sum" #[] with
  | .error e =>
      pure (.error e)
  | .ok _ =>
      match ← checkCommandAvailableIO "openssl" #["version"] with
      | .error e => pure (.error e)
      | .ok _ => pure (.ok ())

inductive ChainTx where
  | publishArtifact (kind : String) (valueHash : Digest)
  | recordCertificate (cert : Digest) (method : String)
  | other
  deriving Repr

structure BlockView where
  txs : List ChainTx
  deriving Repr

def parseSignedTx (c : Canon) : Except String ChainTx := do
  let tx <- canonField c "tx"
  match tx with
  | .tag tag inner =>
      if tag == "publish-artifact" then
        let kind <- canonAsStr (← canonField inner "kind")
        let valueHash <- requireDigest "publish-artifact.value" (← canonAsStr (← canonField inner "value"))
        pure (.publishArtifact kind valueHash)
      else if tag == "record-certificate" then
        let cert <- requireDigest "record-certificate.cert" (← canonAsStr (← canonField inner "cert"))
        let method <- canonAsStr (← canonField inner "method")
        pure (.recordCertificate cert method)
      else
        pure .other
  | _ => pure .other

def parseBlockView (a : ParsedArtifact) : Except String BlockView := do
  if a.kind != "block" then
    .error "artifact is not a block"
  let block <- canonField a.body "block"
  let txsCanon <- canonField block "txs"
  let txs <-
    match txsCanon with
    | .list xs =>
        xs.mapM parseSignedTx
    | _ => .error "block.txs must be list"
  pure { txs := txs }

structure TransitionView where
  before : Digest
  after : Digest
  finality : Option Digest
  deriving Repr

def parseTransitionView (a : ParsedArtifact) : Except String TransitionView := do
  if a.kind != "federation-transition" then
    .error "artifact is not a federation transition"
  let body <- canonExpectTag a.body "federation-transition-v1"
  let before <- requireDigest "transition.before" (← canonAsStr (← canonField body "before"))
  let after <- requireDigest "transition.after" (← canonAsStr (← canonField body "after"))
  let finalityCanon <- canonField body "finality"
  let finality <-
    match finalityCanon with
    | .tag tag inner =>
        if tag == "some" then
          let d <- requireDigest "transition.finality" (← canonAsStr inner)
          pure (some d)
        else
          pure none
    | _ => pure none
  pure { before := before, after := after, finality := finality }

structure FederationStateView where
  trustRoots : Digest
  deriving Repr

def parseFederationStateView (a : ParsedArtifact) : Except String FederationStateView := do
  if a.kind != "federation-state" then
    .error "artifact is not a federation state"
  let body <- canonExpectTag a.body "federation-state-v1"
  let trustRoots <- requireDigest "federation-state.trustRoots" (← canonAsStr (← canonField body "trustRoots"))
  pure { trustRoots := trustRoots }

def containsDigest (xs : List Digest) (d : Digest) : Bool :=
  xs.any (fun x => x == d)

def isMissingCasError (e : String) : Bool :=
  e.startsWith "artifact not in CAS:"

def reprStr {α : Type} [Repr α] (x : α) : String :=
  toString (repr x)

def fnvPrime : UInt64 := 1099511628211

def fnvOffset : UInt64 := 14695981039346656037

def fnv1a (s : String) : UInt64 :=
  s.toUTF8.data.foldl
    (fun h b => (h ^^^ UInt64.ofNat b.toNat) * fnvPrime)
    fnvOffset

def evidenceOf (constitution : KernelConstitution) (query : Query) (value : Value) : Digest :=
  let payload :=
    constitution.kernelId ++ "|" ++ reprStr query ++ "|" ++ reprStr value
  toString (fnv1a payload)

def classifyError (err : String) : KernelResult :=
  if err.startsWith "kernel exhausted:" then
    KernelResult.exhausted (((err.drop 17).trimAscii).toString)
  else
    KernelResult.invalid err

def missingClosureForHistory (ctx : Context) (xs : List Transition) : List Digest :=
  let one := fun (t : Transition) =>
    let missCert := if (ctx.certs.get? t.cert).isSome then [] else [t.cert]
    let missProposal := if (ctx.artifacts.get? t.proposal).isSome then [] else [t.proposal]
    let missManifest := if (ctx.artifacts.get? t.manifest).isSome then [] else [t.manifest]
    missCert ++ missProposal ++ missManifest
  (xs.flatMap one).eraseDups

def replayTransitions
    (federationId : Digest)
    (state : Digest)
    (lastEpoch : Nat)
    (xs : List Transition) : Except String (Digest × Nat × Nat) :=
  match xs with
  | [] => .ok (state, lastEpoch, 0)
  | t :: rest =>
      if t.federationId != federationId then
        .error s!"federation id mismatch at epoch {t.epoch}"
      else if t.before != state then
        .error s!"transition chain break: expected before={state}, got {t.before}"
      else if t.epoch < lastEpoch then
        .error s!"epoch regression: {t.epoch} < {lastEpoch}"
      else
        match replayTransitions federationId t.after t.epoch rest with
        | .error e => .error e
        | .ok (finalState, finalEpoch, n) => .ok (finalState, finalEpoch, n + 1)

def derive (ctx : Context) (constitution : KernelConstitution) (budget : Budget) (query : Query) : KernelResult :=
  let evaluated : Except String Value :=
    match query with
    | .resolve _ digest =>
        match ctx.artifacts.get? digest with
        | some artifact => .ok (.artifact artifact)
        | none => .error s!"artifact not in CAS: {digest}"
    | .verifyCertBinding _ cert proposal manifest =>
        let miss :=
          (if (ctx.certs.get? cert).isSome then [] else [cert]) ++
          (if (ctx.artifacts.get? proposal).isSome then [] else [proposal]) ++
          (if (ctx.artifacts.get? manifest).isSome then [] else [manifest])
        if !miss.isEmpty then
          .error s!"artifact not in CAS: {reprStr miss.eraseDups}"
        else
          match ctx.certs.get? cert with
          | none => .error s!"certificate not in CAS: {cert}"
          | some (cp : CertificateProjection) =>
              if cp.proposal != proposal then
                .error "certificate/proposal mismatch"
              else if cp.manifest != manifest then
                .error "certificate/manifest mismatch"
              else
                .ok (.certBinding cert proposal manifest)
    | .replayHistory _ federationId genesisState =>
        if ctx.history.length > budget.maxSteps then
          .error s!"kernel exhausted: max_steps {budget.maxSteps} exceeded by {ctx.history.length} transitions"
        else if ctx.history.isEmpty then
          .error "no federation transitions published on chain"
        else
          let miss := missingClosureForHistory ctx ctx.history
          if !miss.isEmpty then
            .error s!"artifact not in CAS: {reprStr miss}"
          else
            match replayTransitions federationId genesisState 0 ctx.history with
            | .error e => .error e
            | .ok (finalState, finalEpoch, verifiedTransitions) =>
                .ok (.replayedState {
                  verifiedTransitions := verifiedTransitions
                  finalState := finalState
                  finalEpoch := finalEpoch
                })

  match evaluated with
  | .ok value => .valid value (evidenceOf constitution query value)
  | .error e => classifyError e

namespace CKC

/-- Executable judgment relation: `Sigma;K;B ⊢ q ⇓ r`. -/
def Derives
    (Sigma : Context)
    (K : KernelConstitution)
    (B : Budget)
    (q : Query)
    (r : KernelResult) : Prop :=
  derive Sigma K B q = r

/-- Value-level form of the CKC judgment. -/
def DerivesValue
    (Sigma : Context)
    (K : KernelConstitution)
    (B : Budget)
    (q : Query)
    (v : Value) : Prop :=
  ∃ evidence, Derives Sigma K B q (.valid v evidence)

/-- Executable correspondence (left-to-right and right-to-left by definition). -/
theorem kernelVerify_iff_derives
    (Sigma : Context)
    (K : KernelConstitution)
    (B : Budget)
    (q : Query)
    (r : KernelResult) :
    derive Sigma K B q = r ↔ Derives Sigma K B q r := by
  rfl

/-- Result determinism: one query under one closure/constitution/budget has one result. -/
theorem result_deterministic
    (Sigma : Context)
    (K : KernelConstitution)
    (B : Budget)
    (q : Query)
    (r1 r2 : KernelResult)
    (h1 : Derives Sigma K B q r1)
    (h2 : Derives Sigma K B q r2) :
    r1 = r2 := by
  unfold Derives at h1 h2
  exact h1.symm.trans h2

/-- Semantic determinism for successful derivations. -/
theorem valid_deterministic
    (Sigma : Context)
    (K : KernelConstitution)
    (B : Budget)
    (q : Query)
    (v1 v2 : Value)
    (e1 e2 : Digest)
    (h1 : Derives Sigma K B q (.valid v1 e1))
    (h2 : Derives Sigma K B q (.valid v2 e2)) :
    v1 = v2 ∧ e1 = e2 := by
  have hr : (.valid v1 e1 : KernelResult) = .valid v2 e2 :=
    result_deterministic Sigma K B q (.valid v1 e1) (.valid v2 e2) h1 h2
  cases hr
  exact ⟨rfl, rfl⟩

end CKC

def deriveIO (constitution : KernelConstitution) (budget : Budget) (query : Query) : IO KernelResult := do
  let mkValid := fun (q : Query) (v : Value) => KernelResult.valid v (evidenceOf constitution q v)
  match query with
  | .resolve casRoot digest =>
      let root := System.FilePath.mk casRoot
      match ← readArtifactFromCas root digest with
      | .error e =>
          if e.startsWith "artifact not in CAS:" then
            pure (KernelResult.missing [digest])
          else
            pure (KernelResult.invalid e)
      | .ok a =>
          let k : ArtifactKind :=
            if a.kind == "federation-proposal" then
              .proposal
            else if a.kind == "certificate" then
              .certificate
            else if a.kind == "replica-set-manifest" then
              .replicaManifest
            else
              .other a.kind
          pure (mkValid query (.artifact { digest := digest, kind := k }))
  | .verifyCertBinding casRoot cert proposal manifest =>
      match ← verifyRuntimeDependenciesIO with
      | .error e =>
        return KernelResult.invalid e
      | .ok _ => pure ()

      let root := System.FilePath.mk casRoot
      let mut missing : List Digest := []

      let certExists ← checkDigestExists root cert
      match certExists with
      | .error e =>
          return KernelResult.invalid e
      | .ok false =>
          missing := missing.concat cert
      | .ok true => pure ()

      let proposalExists ← checkDigestExists root proposal
      match proposalExists with
      | .error e =>
          return KernelResult.invalid e
      | .ok false =>
          missing := missing.concat proposal
      | .ok true => pure ()

      let manifestExists ← checkDigestExists root manifest
      match manifestExists with
      | .error e =>
          return KernelResult.invalid e
      | .ok false =>
          missing := missing.concat manifest
      | .ok true => pure ()

      if !missing.isEmpty then
        pure (KernelResult.missing missing.eraseDups)
      else
          match (← readArtifactFromCas root cert), (← readArtifactFromCas root proposal), (← readArtifactFromCas root manifest) with
        | .ok certArtifact, .ok proposalArtifact, .ok manifestArtifact =>
            match parseCertView certArtifact, parseProposalView proposalArtifact, parseManifestView manifestArtifact with
            | .ok certView, .ok proposalView, .ok manifestView =>
              match ← verifyManifestSealsIO manifestView with
              | .error e =>
                  pure (KernelResult.invalid e)
              | .ok _ =>
                  match ← verifyCertSignaturesIO certView manifestView with
                  | .error e =>
                      pure (KernelResult.invalid e)
                  | .ok _ =>
                      match ← verifyManifestDigestBindingIO certView manifestView with
                      | .error e =>
                          pure (KernelResult.invalid e)
                      | .ok _ =>
                          if certView.proposal != proposal then
                            pure (KernelResult.invalid s!"certificate names proposal {certView.proposal}, not {proposal}")
                          else if certView.transition != proposalView.transition then
                            pure (KernelResult.invalid "certificate transition projection does not match proposal")
                          else if certView.state != proposalView.after then
                            pure (KernelResult.invalid "certificate state projection does not match proposal.after")
                          else if certView.previousState != proposalView.before then
                            pure (KernelResult.invalid "certificate previousState projection does not match proposal.before")
                          else if certView.epoch != proposalView.epoch then
                            pure (KernelResult.invalid "certificate epoch projection does not match proposal")
                          else if certView.replicaSet != proposalView.replicaSet then
                            pure (KernelResult.invalid "certificate replicaSet projection does not match proposal")
                          else if certView.federationId != proposalView.federationId then
                            pure (KernelResult.invalid "certificate federationId projection does not match proposal")
                          else
                            pure (mkValid query (.certBinding cert proposal manifest))
            | .error e, _, _ =>
                pure (KernelResult.invalid e)
            | _, .error e, _ =>
                pure (KernelResult.invalid e)
            | _, _, .error e =>
                pure (KernelResult.invalid e)
        | .error e, _, _ =>
            pure (KernelResult.invalid e)
        | _, .error e, _ =>
            pure (KernelResult.invalid e)
        | _, _, .error e =>
            pure (KernelResult.invalid e)
  | .replayHistory nodeRoot federationId genesisState =>
      match ← verifyRuntimeDependenciesIO with
      | .error e =>
        return KernelResult.invalid e
      | .ok _ => pure ()

      match ← readChainDigests nodeRoot with
      | .error e =>
          pure (classifyError e)
      | .ok chainDigests =>
          let root := System.FilePath.mk nodeRoot
          let mut transitionDigests : List Digest := []
          let mut recordedCerts : List Digest := []

          for blockDigest in chainDigests do
            match ← readArtifactFromCas root blockDigest with
            | .error e =>
                if isMissingCasError e then
                  return KernelResult.missing [blockDigest]
                else
                  return KernelResult.invalid e
            | .ok blockArtifact =>
                match parseBlockView blockArtifact with
                | .error e =>
                    return KernelResult.invalid e
                | .ok block =>
                    for tx in block.txs do
                      match tx with
                      | .publishArtifact kind valueHash =>
                          if kind == "federation-transition" then
                            transitionDigests := transitionDigests.concat valueHash
                      | .recordCertificate cert method =>
                          if method == "federation-finality" then
                            recordedCerts := recordedCerts.concat cert
                      | .other => pure ()

          if transitionDigests.isEmpty then
            pure (KernelResult.invalid "no federation transitions published on chain")
          else if transitionDigests.length > budget.maxSteps then
            pure (KernelResult.exhausted s!"max_steps {budget.maxSteps} exceeded by {transitionDigests.length} transitions")
          else
            let mut expectedBefore := genesisState
            let mut lastState := genesisState
            let mut lastEpoch : Nat := 0

            for transitionDigest in transitionDigests do
              match ← readArtifactFromCas root transitionDigest with
              | .error e =>
                  if isMissingCasError e then
                    return KernelResult.missing [transitionDigest]
                  else
                    return KernelResult.invalid e
              | .ok transitionArtifact =>
                  match parseTransitionView transitionArtifact with
                  | .error e =>
                      return KernelResult.invalid e
                  | .ok transition =>
                      if transition.before != expectedBefore then
                        return KernelResult.invalid s!"federation history: transition {transitionDigest} does not chain from {expectedBefore}"

                      let certDigest <-
                        match transition.finality with
                        | some d => pure d
                        | none =>
                            return KernelResult.invalid s!"federation history: transition {transitionDigest} missing finality"

                      if !(containsDigest recordedCerts certDigest) then
                        return KernelResult.invalid s!"federation history: certificate {certDigest} not anchored in ledger record-certificate txs"

                      let certArtifact <-
                        match ← readArtifactFromCas root certDigest with
                        | .error e =>
                            if isMissingCasError e then
                              return KernelResult.missing [certDigest]
                            else
                              return KernelResult.invalid e
                        | .ok a => pure a

                      let certView <-
                        match parseCertView certArtifact with
                        | .ok v => pure v
                        | .error e => return KernelResult.invalid e

                      if certView.federationId != federationId then
                        return KernelResult.invalid "federation finality: federation id mismatch"

                      let proposalArtifact <-
                        match ← readArtifactFromCas root certView.proposal with
                        | .error e =>
                            if isMissingCasError e then
                              return KernelResult.missing [certView.proposal]
                            else
                              return KernelResult.invalid e
                        | .ok a => pure a

                      let proposalView <-
                        match parseProposalView proposalArtifact with
                        | .ok v => pure v
                        | .error e => return KernelResult.invalid e

                      let beforeStateArtifact <-
                        match ← readArtifactFromCas root transition.before with
                        | .error e =>
                            if isMissingCasError e then
                              return KernelResult.missing [transition.before]
                            else
                              return KernelResult.invalid e
                        | .ok a => pure a

                      let beforeState <-
                        match parseFederationStateView beforeStateArtifact with
                        | .ok v => pure v
                        | .error e => return KernelResult.invalid e

                      let afterStateArtifact <-
                        match ← readArtifactFromCas root transition.after with
                        | .error e =>
                            if isMissingCasError e then
                              return KernelResult.missing [transition.after]
                            else
                              return KernelResult.invalid e
                        | .ok a => pure a

                      match parseFederationStateView afterStateArtifact with
                      | .error e =>
                          return KernelResult.invalid e
                      | .ok _ => pure ()

                      let manifestArtifact <-
                        match ← readArtifactFromCas root beforeState.trustRoots with
                        | .error e =>
                            if isMissingCasError e then
                              return KernelResult.missing [beforeState.trustRoots]
                            else
                              return KernelResult.invalid e
                        | .ok a => pure a

                      let manifestView <-
                        match parseManifestView manifestArtifact with
                        | .error e => return KernelResult.invalid e
                        | .ok v => pure v

                      match ← verifyManifestSealsIO manifestView with
                      | .error e =>
                        return KernelResult.invalid e
                      | .ok _ =>
                        match ← verifyCertSignaturesIO certView manifestView with
                        | .error e =>
                          return KernelResult.invalid e
                        | .ok _ =>
                          match ← verifyManifestDigestBindingIO certView manifestView with
                          | .error e => return KernelResult.invalid e
                          | .ok _ => pure ()

                      if certView.transition != proposalView.transition then
                        return KernelResult.invalid "certificate transition projection does not match proposal"
                      else if certView.state != proposalView.after then
                        return KernelResult.invalid "certificate state projection does not match proposal.after"
                      else if certView.previousState != proposalView.before then
                        return KernelResult.invalid "certificate previousState projection does not match proposal.before"
                      else if certView.epoch != proposalView.epoch then
                        return KernelResult.invalid "certificate epoch projection does not match proposal"
                      else if certView.replicaSet != proposalView.replicaSet then
                        return KernelResult.invalid "certificate replicaSet projection does not match proposal"
                      else if certView.federationId != proposalView.federationId then
                        return KernelResult.invalid "certificate federationId projection does not match proposal"
                      else if proposalView.transition != transitionDigest then
                        return KernelResult.invalid s!"federation history: proposal transition does not match published transition {transitionDigest}"
                      else if proposalView.before != transition.before || proposalView.after != transition.after then
                        return KernelResult.invalid s!"federation history: proposal before/after does not match transition {transitionDigest}"
                      else if certView.state != transition.after then
                        return KernelResult.invalid "federation finality: certificate subject is not transition.after"
                      else if certView.previousState != transition.before then
                        return KernelResult.invalid "federation finality: certificate predecessor is not transition.before"
                      else if certView.epoch < 0 then
                        return KernelResult.invalid "federation finality: epoch must be non-negative"
                      else
                        let epochNat := Int.toNat certView.epoch
                        if epochNat < lastEpoch then
                          return KernelResult.invalid s!"epoch regression: {epochNat} < {lastEpoch}"
                        else
                          expectedBefore := transition.after
                          lastState := transition.after
                          lastEpoch := epochNat

            pure (mkValid query (.replayedState {
              verifiedTransitions := transitionDigests.length
              finalState := lastState
              finalEpoch := lastEpoch
            }))

def KernelResult.isValid : KernelResult → Bool
  | .valid _ _ => true
  | _ => false

-- Minimal executable examples that also act as compile-time regression checks.
def demoContext : Context :=
  let proposalDigest := "proposal-1"
  let manifestDigest := "manifest-1"
  let certDigest := "cert-1"
  let afterDigest := "state-1"
  let artifacts : Std.HashMap Digest Artifact :=
    ((∅ : Std.HashMap Digest Artifact).insert proposalDigest { digest := proposalDigest, kind := .proposal })
      |>.insert manifestDigest { digest := manifestDigest, kind := .replicaManifest }
  let certs : Std.HashMap Digest CertificateProjection :=
    ((∅ : Std.HashMap Digest CertificateProjection).insert certDigest {
      cert := certDigest
      proposal := proposalDigest
      manifest := manifestDigest
      federationId := "fed-1"
      epoch := 1
    })
  let history := [{
    before := "genesis"
    after := afterDigest
    cert := certDigest
    proposal := proposalDigest
    manifest := manifestDigest
    federationId := "fed-1"
    epoch := 1
  }]
  { artifacts := artifacts, certs := certs, history := history }

end VerifierLean
