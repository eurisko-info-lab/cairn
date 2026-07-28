# PR34 Envelope Schema

This page fixes the wire shape for the initial PR34 closed-world package and
verdict envelope used by cross-implementation conformance.

## Canonical tags

- Graph package tag: `pr34-graph-package-v1`
- Verdict envelope tag: `pr34-verdict-envelope-v1`
- Successor link tag: `pr34-successor-link-v1`

## Graph package (`Pr34GraphPackage`)

Canonical body map fields:

- `kernelConstitution: string` (digest hex)
- `artifactClosure: string` (digest hex)
- `machineClosure: string` (digest hex)
- `runtimeClosure: string` (digest hex)
- `acceptanceClosure: string` (digest hex)
- `repositoryRoot: string` (digest hex)
- `finalizedHistory: string` (digest hex)
- `evidenceClosure: string` (digest hex)

Digest identity:

- package digest is `Digest.of(canon)`

Storage wrapper:

- stored as `Artifact(kind = Trace, body = canon)`

## Verdict envelope (`Pr34VerdictEnvelope`)

Canonical body map fields:

- `kernelConstitution: string` (digest hex)
- `graphPackage: string` (digest hex of `Pr34GraphPackage`)
- `verdictClass: string` in `{ valid, invalid, missing, exhausted }`
- `state: option<string digest>`
- `evidence: option<string digest>`
- `resourceUse: map`

`resourceUse` map fields:

- `steps: int`
- `bytesRead: int`
- `wallMicros: int`

Option encoding:

- none: `CTag("none", CInt(0))`
- some digest: `CTag("some", CStr(<digest-hex>))`

Digest identity:

- verdict digest is `Digest.of(canon)`

Storage wrapper:

- stored as `Artifact(kind = Trace, body = canon)`

## CKC interop mapping

Current runtime bridge (`Pr34EnvelopeInterop.fromCkc`) maps:

- `verdictClass` from CKC result family
- `state` from replay-valid result only (`Value.ReplayedState.finalState`)
- `evidence` from CKC valid result evidence
- `graphPackage` from caller-provided package digest
- `kernelConstitution` from constitution kernel id digest

This bridge is intentionally narrow for the scaffold phase. Later slices expand
state projection and cross-language envelope emission.

## Staircase scaffold (`Pr34SuccessorLink`)

Current scaffold uses a dedicated successor-link object:

- tag: `pr34-successor-link-v1`
- body fields:
	- `predecessorPackage: string` (digest hex)
	- `successorPackage: string` (digest hex)
	- `upgradeDelta: string` (digest hex)

Current validator shape (`validateTwoStep` / `validate_two_step` /
`Pr34StaircaseValidateTwoStep`) checks:

- `g0` and `g1` verdict classes are `valid`
- `g0.graphPackage == predecessorPackage`
- `g1.graphPackage == successorPackage`
- both verdicts carry state and evidence
- predecessor/successor package digests are distinct
- all referenced digests are valid 64-hex values (including CLI inputs)

## Staircase package-v2 extension (planned)

After fixture hardening, generation-link data may move into a package version
bump (for example, `pr34-graph-package-v2`) with:

- `generation: int`
- `predecessorPackage: option<digest>`
- `upgradeDelta: option<digest>`

Closure criterion remains:

- independent reconstruction of `G0` to `S0`
- governed/accepted/finalized transition to `G1`
- independent reconstruction of `G1` to `S1` with matching canonical verdicts.
