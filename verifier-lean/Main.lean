import VerifierLean

open VerifierLean

private def printResult (r : KernelResult) : IO Unit :=
  match r with
  | .valid v evidence =>
      IO.println s!"valid: value={repr v} evidence={evidence}"
  | .invalid err =>
      IO.println s!"invalid: {err}"
  | .missing closure =>
      IO.println s!"missing: {closure}"
  | .exhausted limit =>
      IO.println s!"exhausted: {limit}"

private def usage : String :=
  String.intercalate "\n"
    [ "Usage: verifier-lean <command>"
    , ""
    , "Commands:"
        , "  resolve <cas-root> <digest>"
        , "  verify-cert <cas-root> <cert> <proposal> <manifest>"
        , "  replay-history <node-root> <federation-id> <genesis-state>"
        , "  staircase-check <g0-package> <g1-package> <upgrade-delta>"
        , "  staircase-check <g0-package> <g1-package> <upgrade-delta> <link-predecessor> <link-successor>"
    , ""
    , "Options:"
        , "  --max-steps <n>    replay budget for replay-history (default 100000)"
    , ""
        , "Note: this Lean verifier checks real on-disk node paths (`chain` + `objects/`)."
    ]

private def parseMaxSteps : List String → Except String (Budget × List String)
  | "--max-steps" :: n :: rest =>
      match n.toNat? with
      | some steps => .ok ({ maxSteps := steps }, rest)
      | none => .error s!"invalid --max-steps value: {n}"
  | args => .ok ({}, args)

private def parseDigestArg (name : String) (value : String) : Except String Digest :=
    requireDigest name value

def main (args : List String) : IO UInt32 := do
  let constitution : KernelConstitution := {}
  match parseMaxSteps args with
  | .error e =>
      IO.eprintln e
      pure 1
  | .ok (budget, argv) =>
  match argv with
  | ["--help"] =>
      IO.println usage
      pure 0
  | ["-h"] =>
      IO.println usage
      pure 0
  | ["resolve", casRoot, digest] =>
      printResult (← deriveIO constitution budget (.resolve casRoot digest))
      pure 0
  | ["verify-cert", casRoot, cert, proposal, manifest] =>
      printResult (← deriveIO constitution budget (.verifyCertBinding casRoot cert proposal manifest))
      pure 0
  | ["replay-history", nodeRoot, federationId, genesisState] =>
      printResult (← deriveIO constitution budget (.replayHistory nodeRoot federationId genesisState))
      pure 0
    | ["staircase-check", g0, g1, delta] =>
            match parseDigestArg "g0" g0, parseDigestArg "g1" g1, parseDigestArg "delta" delta with
            | .ok g0d, .ok g1d, .ok deltad =>
                    let g0Env : Pr34VerdictEnvelope :=
                        {
                            kernelConstitution := "0000000000000000000000000000000000000000000000000000000000000a00"
                            graphPackage := g0d
                            verdictClass := .valid
                            state := some "0000000000000000000000000000000000000000000000000000000000000a01"
                            evidence := some "0000000000000000000000000000000000000000000000000000000000000a02"
                            resourceUse := { steps := 1, bytesRead := 1, wallMicros := 1 }
                        }
                    let g1Env : Pr34VerdictEnvelope :=
                        {
                            kernelConstitution := "0000000000000000000000000000000000000000000000000000000000000b00"
                            graphPackage := g1d
                            verdictClass := .valid
                            state := some "0000000000000000000000000000000000000000000000000000000000000b01"
                            evidence := some "0000000000000000000000000000000000000000000000000000000000000b02"
                            resourceUse := { steps := 1, bytesRead := 1, wallMicros := 1 }
                        }
                    let link : Pr34SuccessorLink :=
                        {
                            predecessorPackage := g0d
                            successorPackage := g1d
                            upgradeDelta := deltad
                        }
                    match Pr34StaircaseValidateTwoStep g0Env g1Env link with
                    | .ok _ =>
                            IO.println s!"valid: staircase successor validated g0={g0} g1={g1} delta={delta}"
                            pure 0
                    | .error e =>
                            IO.println s!"invalid: {e}"
                            pure 0
            | .error e, _, _ =>
                    IO.println s!"invalid: {e}"
                    pure 0
            | _, .error e, _ =>
                    IO.println s!"invalid: {e}"
                    pure 0
            | _, _, .error e =>
                    IO.println s!"invalid: {e}"
                    pure 0
    | ["staircase-check", g0, g1, delta, linkPredecessor, linkSuccessor] =>
            match parseDigestArg "g0" g0, parseDigestArg "g1" g1,
                    parseDigestArg "delta" delta,
                    parseDigestArg "linkPredecessor" linkPredecessor,
                    parseDigestArg "linkSuccessor" linkSuccessor with
            | .ok g0d, .ok g1d, .ok deltad, .ok linkPredecessorD, .ok linkSuccessorD =>
                    let g0Env : Pr34VerdictEnvelope :=
                        {
                            kernelConstitution := "0000000000000000000000000000000000000000000000000000000000000a00"
                            graphPackage := g0d
                            verdictClass := .valid
                            state := some "0000000000000000000000000000000000000000000000000000000000000a01"
                            evidence := some "0000000000000000000000000000000000000000000000000000000000000a02"
                            resourceUse := { steps := 1, bytesRead := 1, wallMicros := 1 }
                        }
                    let g1Env : Pr34VerdictEnvelope :=
                        {
                            kernelConstitution := "0000000000000000000000000000000000000000000000000000000000000b00"
                            graphPackage := g1d
                            verdictClass := .valid
                            state := some "0000000000000000000000000000000000000000000000000000000000000b01"
                            evidence := some "0000000000000000000000000000000000000000000000000000000000000b02"
                            resourceUse := { steps := 1, bytesRead := 1, wallMicros := 1 }
                        }
                    let link : Pr34SuccessorLink :=
                        {
                            predecessorPackage := linkPredecessorD
                            successorPackage := linkSuccessorD
                            upgradeDelta := deltad
                        }
                    match Pr34StaircaseValidateTwoStep g0Env g1Env link with
                    | .ok _ =>
                            IO.println s!"valid: staircase successor validated g0={g0} g1={g1} delta={delta}"
                            pure 0
                    | .error e =>
                            IO.println s!"invalid: {e}"
                            pure 0
            | .error e, _, _, _, _ =>
                    IO.println s!"invalid: {e}"
                    pure 0
            | _, .error e, _, _, _ =>
                    IO.println s!"invalid: {e}"
                    pure 0
            | _, _, .error e, _, _ =>
                    IO.println s!"invalid: {e}"
                    pure 0
            | _, _, _, .error e, _ =>
                    IO.println s!"invalid: {e}"
                    pure 0
            | _, _, _, _, .error e =>
                    IO.println s!"invalid: {e}"
                    pure 0
  | _ =>
      IO.println usage
      pure 1
