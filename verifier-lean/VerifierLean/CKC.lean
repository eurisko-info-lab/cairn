import Std

namespace VerifierLean

abbrev Digest := String

inductive ArtifactKind where
  | proposal
  | certificate
  | replicaManifest
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

def deriveIO (constitution : KernelConstitution) (budget : Budget) (query : Query) : IO KernelResult := do
  let mkValid := fun (q : Query) (v : Value) => KernelResult.valid v (evidenceOf constitution q v)
  match query with
  | .resolve casRoot digest =>
      let root := System.FilePath.mk casRoot
      match ← checkDigestExists root digest with
      | .error e =>
          pure (KernelResult.invalid e)
      | .ok true =>
          pure (mkValid query (.artifact { digest := digest, kind := .proposal }))
      | .ok false =>
          pure (KernelResult.missing [digest])
  | .verifyCertBinding casRoot cert proposal manifest =>
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
        pure (mkValid query (.certBinding cert proposal manifest))
  | .replayHistory nodeRoot _federationId genesisState =>
      match ← readChainDigests nodeRoot with
      | .error e =>
          pure (classifyError e)
      | .ok chainDigests =>
          if chainDigests.length > budget.maxSteps then
            pure (KernelResult.exhausted s!"max_steps {budget.maxSteps} exceeded by {chainDigests.length} transitions")
          else
            let root := System.FilePath.mk nodeRoot
            let mut missing : List Digest := []
            for d in chainDigests do
              let ex ← checkDigestExists root d
              match ex with
              | .error e =>
                  return KernelResult.invalid e
              | .ok false =>
                  missing := missing.concat d
              | .ok true =>
                  pure ()

            if !missing.isEmpty then
              pure (KernelResult.missing missing.eraseDups)
            else
              let finalDigest := chainDigests.getLastD genesisState
              pure (mkValid query (.replayedState {
                verifiedTransitions := chainDigests.length
                finalState := finalDigest
                finalEpoch := chainDigests.length
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
