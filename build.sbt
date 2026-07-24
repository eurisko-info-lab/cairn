ThisBuild / scalaVersion := "3.3.4"
ThisBuild / organization := "cairn"
ThisBuild / version := "0.1.0"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:imports")

val munit = "org.scalameta" %% "munit" % "1.0.2" % Test

lazy val kernel = project.in(file("kernel"))
  .settings(libraryDependencies += munit)

// Ledger wire types (Tx/SignedTx/Block) — split out of kernel-container so
// system-interface can depend on them without pulling in Merkle-backed
// LedgerState/LedgerKernel, which only system-handler needs.
lazy val ledgerTypes = project.in(file("container/ledger-types"))
  .dependsOn(kernel)
  .settings(libraryDependencies += munit)

// Container-only kernel primitives (Merkle/Ledger/BFT/branch+replica-set
// manifests/authority certs/domain agreement/identity resolution) — needed by
// system-handler, not by core/user/proof. Split out of kernel so container
// and content don't both have to depend on the whole thing.
lazy val kernelContainer = project.in(file("container/kernel-container"))
  .dependsOn(kernel, ledgerTypes)
  .settings(libraryDependencies += munit)

// Content-only kernel primitives (Rename/Rewrite/Checker) — needed by core,
// not by system-interface/system-handler. Split out of kernel alongside
// kernelContainer for the same reason.
lazy val kernelRewrite = project.in(file("content/kernel-rewrite"))
  .dependsOn(kernel)
  .settings(libraryDependencies += munit)

// Shared effect-contract vocabulary (Cas/Filesystem/Process/.../PackAccess/
// AuthorizationProver) — depended on by both container (implements) and
// content (references PackAccess), so it lives beside kernel rather than
// under container/. Package cairn.systeminterface unchanged.
lazy val contracts = project.in(file("contracts"))
  .dependsOn(kernel)
  .settings(libraryDependencies += munit)

// MIGRATION-PLAN.md Phase 1: System Interface vs Handler.
// Now holds only LedgerTransport (needs ledgerTypes' SignedTx/Block); every
// other effect contract moved to `contracts`.
lazy val systemInterface = project.in(file("container/system-interface"))
  .dependsOn(kernel, ledgerTypes)
  .settings(libraryDependencies += munit)

lazy val systemHandler = project.in(file("container/system-handler"))
  .dependsOn(kernel, kernelContainer, systemInterface, contracts)
  .settings(libraryDependencies += munit)

// MIGRATION-PLAN.md Phase 2: pure proposal machinery.
lazy val core = project.in(file("content/core"))
  .dependsOn(kernel, kernelRewrite)
  .settings(libraryDependencies += munit)

// MIGRATION-PLAN.md Phase 6: User — languages/policies/workflows; never imports handlers.
lazy val user = project.in(file("content/user"))
  .dependsOn(kernel, core, contracts)
  .settings(libraryDependencies += munit)

// MIGRATION-PLAN.md Phase 8: composition root (PackLoader, CLI wiring).
lazy val runtime = project.in(file("app/runtime"))
  .dependsOn(user, systemHandler, core, kernel, systemInterface, contracts)
  .settings(libraryDependencies += munit)

lazy val proof = project.in(file("content/proof"))
  .dependsOn(core, kernel)
  .settings(libraryDependencies += munit)

lazy val rosetta = project.in(file("app/rosetta"))
  .dependsOn(proof, core, systemHandler)
  .settings(libraryDependencies += munit)

lazy val surface = project.in(file("app/surface"))
  .dependsOn(proof, runtime, systemHandler)
  .settings(libraryDependencies += munit)

// Demos / entrypoints — may use the full stack (runtime composition).
lazy val examples = project.in(file("app/examples"))
  .dependsOn(surface, user, runtime)
  .settings(
    libraryDependencies += munit,
    assembly / mainClass := Some("cairn.examples.Main"),
    assembly / assemblyJarName := "cairn.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    },
  )

lazy val tests = project.in(file("app/tests"))
  .dependsOn(examples, rosetta, runtime, user)
  .settings(libraryDependencies += munit)

lazy val root = project.in(file("."))
  .aggregate(
    kernel, ledgerTypes, kernelContainer, kernelRewrite, contracts, core, systemInterface,
    systemHandler, user, runtime, proof, rosetta, surface, examples, tests)
  .settings(publish / skip := true)
