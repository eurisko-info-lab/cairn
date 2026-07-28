import VerifierLean.CKC

namespace VerifierLean

structure Pr34GraphPackage where
  kernelConstitution : Digest
  artifactClosure : Digest
  machineClosure : Digest
  runtimeClosure : Digest
  acceptanceClosure : Digest
  repositoryRoot : Digest
  finalizedHistory : Digest
  evidenceClosure : Digest
  deriving Repr, BEq, DecidableEq

def Pr34GraphPackage.canon (g : Pr34GraphPackage) : Canon :=
  .tag "pr34-graph-package-v1" (.map [
    ("kernelConstitution", .str g.kernelConstitution),
    ("artifactClosure", .str g.artifactClosure),
    ("machineClosure", .str g.machineClosure),
    ("runtimeClosure", .str g.runtimeClosure),
    ("acceptanceClosure", .str g.acceptanceClosure),
    ("repositoryRoot", .str g.repositoryRoot),
    ("finalizedHistory", .str g.finalizedHistory),
    ("evidenceClosure", .str g.evidenceClosure)
  ])

def Pr34GraphPackage.fromCanon (c : Canon) : Except String Pr34GraphPackage := do
  let body <- canonExpectTag c "pr34-graph-package-v1"
  let kernelConstitution <- requireDigest "pr34.kernelConstitution" (← canonAsStr (← canonField body "kernelConstitution"))
  let artifactClosure <- requireDigest "pr34.artifactClosure" (← canonAsStr (← canonField body "artifactClosure"))
  let machineClosure <- requireDigest "pr34.machineClosure" (← canonAsStr (← canonField body "machineClosure"))
  let runtimeClosure <- requireDigest "pr34.runtimeClosure" (← canonAsStr (← canonField body "runtimeClosure"))
  let acceptanceClosure <- requireDigest "pr34.acceptanceClosure" (← canonAsStr (← canonField body "acceptanceClosure"))
  let repositoryRoot <- requireDigest "pr34.repositoryRoot" (← canonAsStr (← canonField body "repositoryRoot"))
  let finalizedHistory <- requireDigest "pr34.finalizedHistory" (← canonAsStr (← canonField body "finalizedHistory"))
  let evidenceClosure <- requireDigest "pr34.evidenceClosure" (← canonAsStr (← canonField body "evidenceClosure"))
  pure {
    kernelConstitution := kernelConstitution
    artifactClosure := artifactClosure
    machineClosure := machineClosure
    runtimeClosure := runtimeClosure
    acceptanceClosure := acceptanceClosure
    repositoryRoot := repositoryRoot
    finalizedHistory := finalizedHistory
    evidenceClosure := evidenceClosure
  }

inductive Pr34VerdictClass where
  | valid
  | invalid
  | missing
  | exhausted
  deriving Repr, BEq, DecidableEq

def Pr34VerdictClass.wire : Pr34VerdictClass → String
  | .valid => "valid"
  | .invalid => "invalid"
  | .missing => "missing"
  | .exhausted => "exhausted"

def Pr34VerdictClass.parse (s : String) : Except String Pr34VerdictClass :=
  match s with
  | "valid" => .ok .valid
  | "invalid" => .ok .invalid
  | "missing" => .ok .missing
  | "exhausted" => .ok .exhausted
  | _ => .error s!"invalid pr34 verdict class: {s}"

structure Pr34ResourceUse where
  steps : Nat
  bytesRead : Nat
  wallMicros : Nat
  deriving Repr, BEq, DecidableEq

def Pr34ResourceUse.canon (r : Pr34ResourceUse) : Canon :=
  .map [
    ("steps", .int (Int.ofNat r.steps)),
    ("bytesRead", .int (Int.ofNat r.bytesRead)),
    ("wallMicros", .int (Int.ofNat r.wallMicros))
  ]

def Pr34ResourceUse.fromCanon (c : Canon) : Except String Pr34ResourceUse := do
  let steps <- canonAsInt (← canonField c "steps")
  let bytesRead <- canonAsInt (← canonField c "bytesRead")
  let wallMicros <- canonAsInt (← canonField c "wallMicros")
  if steps < 0 || bytesRead < 0 || wallMicros < 0 then
    .error "pr34 resource use fields must be non-negative"
  else
    .ok {
      steps := Int.toNat steps
      bytesRead := Int.toNat bytesRead
      wallMicros := Int.toNat wallMicros
    }

structure Pr34VerdictEnvelope where
  kernelConstitution : Digest
  graphPackage : Digest
  verdictClass : Pr34VerdictClass
  state : Option Digest
  evidence : Option Digest
  resourceUse : Pr34ResourceUse
  deriving Repr, BEq, DecidableEq

def optionDigestCanon (d : Option Digest) : Canon :=
  match d with
  | none => .tag "none" (.int 0)
  | some h => .tag "some" (.str h)

def optionDigestFromCanon (c : Canon) : Except String (Option Digest) :=
  match c with
  | .tag "none" _ => .ok none
  | .tag "some" (.str h) =>
      match requireDigest "optionDigest" h with
      | .ok d => .ok (some d)
      | .error e => .error e
  | _ => .error "expected option digest"

def Pr34VerdictEnvelope.canon (v : Pr34VerdictEnvelope) : Canon :=
  .tag "pr34-verdict-envelope-v1" (.map [
    ("kernelConstitution", .str v.kernelConstitution),
    ("graphPackage", .str v.graphPackage),
    ("verdictClass", .str v.verdictClass.wire),
    ("state", optionDigestCanon v.state),
    ("evidence", optionDigestCanon v.evidence),
    ("resourceUse", v.resourceUse.canon)
  ])

def Pr34VerdictEnvelope.fromCanon (c : Canon) : Except String Pr34VerdictEnvelope := do
  let body <- canonExpectTag c "pr34-verdict-envelope-v1"
  let kernelConstitution <- requireDigest "pr34.kernelConstitution" (← canonAsStr (← canonField body "kernelConstitution"))
  let graphPackage <- requireDigest "pr34.graphPackage" (← canonAsStr (← canonField body "graphPackage"))
  let verdictClass <- Pr34VerdictClass.parse (← canonAsStr (← canonField body "verdictClass"))
  let state <- optionDigestFromCanon (← canonField body "state")
  let evidence <- optionDigestFromCanon (← canonField body "evidence")
  let resourceUse <- Pr34ResourceUse.fromCanon (← canonField body "resourceUse")
  pure {
    kernelConstitution := kernelConstitution
    graphPackage := graphPackage
    verdictClass := verdictClass
    state := state
    evidence := evidence
    resourceUse := resourceUse
  }

structure Pr34SuccessorLink where
  predecessorPackage : Digest
  successorPackage : Digest
  upgradeDelta : Digest
  deriving Repr, BEq, DecidableEq

def Pr34SuccessorLink.canon (l : Pr34SuccessorLink) : Canon :=
  .tag "pr34-successor-link-v1" (.map [
    ("predecessorPackage", .str l.predecessorPackage),
    ("successorPackage", .str l.successorPackage),
    ("upgradeDelta", .str l.upgradeDelta)
  ])

def Pr34SuccessorLink.fromCanon (c : Canon) : Except String Pr34SuccessorLink := do
  let body <- canonExpectTag c "pr34-successor-link-v1"
  let predecessorPackage <- requireDigest "pr34.predecessorPackage" (← canonAsStr (← canonField body "predecessorPackage"))
  let successorPackage <- requireDigest "pr34.successorPackage" (← canonAsStr (← canonField body "successorPackage"))
  let upgradeDelta <- requireDigest "pr34.upgradeDelta" (← canonAsStr (← canonField body "upgradeDelta"))
  pure {
    predecessorPackage := predecessorPackage
    successorPackage := successorPackage
    upgradeDelta := upgradeDelta
  }

def Pr34StaircaseValidateTwoStep
    (g0 : Pr34VerdictEnvelope)
    (g1 : Pr34VerdictEnvelope)
    (link : Pr34SuccessorLink) : Except String Unit := do
  if g0.verdictClass != .valid then
    .error "g0 verdict is not valid"
  else if g1.verdictClass != .valid then
    .error "g1 verdict is not valid"
  else if g0.graphPackage != link.predecessorPackage then
    .error "g0 package does not match successor link predecessor"
  else if g1.graphPackage != link.successorPackage then
    .error "g1 package does not match successor link successor"
  else if g0.state.isNone then
    .error "g0 valid verdict is missing state"
  else if g1.state.isNone then
    .error "g1 valid verdict is missing state"
  else if g0.evidence.isNone then
    .error "g0 valid verdict is missing evidence"
  else if g1.evidence.isNone then
    .error "g1 valid verdict is missing evidence"
  else if link.predecessorPackage == link.successorPackage then
    .error "successor package must differ from predecessor package"
  else
    .ok ()

-- compile-time round-trip checks for the scaffold
def demoPr34Graph : Pr34GraphPackage :=
  {
    kernelConstitution := "0000000000000000000000000000000000000000000000000000000000000001"
    artifactClosure := "0000000000000000000000000000000000000000000000000000000000000002"
    machineClosure := "0000000000000000000000000000000000000000000000000000000000000003"
    runtimeClosure := "0000000000000000000000000000000000000000000000000000000000000004"
    acceptanceClosure := "0000000000000000000000000000000000000000000000000000000000000005"
    repositoryRoot := "0000000000000000000000000000000000000000000000000000000000000006"
    finalizedHistory := "0000000000000000000000000000000000000000000000000000000000000007"
    evidenceClosure := "0000000000000000000000000000000000000000000000000000000000000008"
  }

def demoPr34RoundTrip : Except String Pr34GraphPackage :=
  Pr34GraphPackage.fromCanon demoPr34Graph.canon

def demoPr34Link : Pr34SuccessorLink :=
  {
    predecessorPackage := "0000000000000000000000000000000000000000000000000000000000000010"
    successorPackage := "0000000000000000000000000000000000000000000000000000000000000011"
    upgradeDelta := "0000000000000000000000000000000000000000000000000000000000000012"
  }

def demoPr34LinkRoundTrip : Except String Pr34SuccessorLink :=
  Pr34SuccessorLink.fromCanon demoPr34Link.canon

def demoPr34StaircaseOk : Except String Unit :=
  let g0 : Pr34VerdictEnvelope :=
    {
      kernelConstitution := "0000000000000000000000000000000000000000000000000000000000000100"
      graphPackage := demoPr34Link.predecessorPackage
      verdictClass := .valid
      state := some "0000000000000000000000000000000000000000000000000000000000000101"
      evidence := some "0000000000000000000000000000000000000000000000000000000000000102"
      resourceUse := { steps := 1, bytesRead := 1, wallMicros := 1 }
    }
  let g1 : Pr34VerdictEnvelope :=
    {
      kernelConstitution := "0000000000000000000000000000000000000000000000000000000000000200"
      graphPackage := demoPr34Link.successorPackage
      verdictClass := .valid
      state := some "0000000000000000000000000000000000000000000000000000000000000201"
      evidence := some "0000000000000000000000000000000000000000000000000000000000000202"
      resourceUse := { steps := 1, bytesRead := 1, wallMicros := 1 }
    }
  Pr34StaircaseValidateTwoStep g0 g1 demoPr34Link

 end VerifierLean
