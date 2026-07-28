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
    , ""
        , "Note: this Lean verifier checks real on-disk node paths (`chain` + `objects/`)."
    ]

def main (args : List String) : IO UInt32 := do
  let constitution : KernelConstitution := {}
  let budget : Budget := {}
  match args with
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
  | _ =>
      IO.println usage
      pure 1
