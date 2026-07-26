# Pack and application ecosystem

An ecosystem release is a content-addressed `ecosystem-bundle` signed over the
domain `cairn-ecosystem-release-v1`. It binds:

- namespace and semantic version;
- pack or application root digest;
- declared migration artifacts and predecessor releases;
- publisher identity and exact Ed25519 public key.

Discovery is trust-gated. `EcosystemTrustPolicy` binds publisher names to keys,
namespace owners, revoked bundle digests, and whether ledger publication is
mandatory. A registry verifies the signature and policy before exposing a
version, rejects two different releases at one namespace/version, and computes
shortest migration routes only from migrations in accepted releases.

Publication stores the signed bundle in CAS and publishes its typed key on the
ledger. Replication pulls the bundle through the same recursive,
digest-verifying dependency installer used by artifact-only startup, so the
pack/application root, migrations, and predecessor bundle graph arrive as one
auditable closure.
