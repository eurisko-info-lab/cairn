ThisBuild / scalaVersion := "3.3.4"
ThisBuild / organization := "cairn"
ThisBuild / version := "0.1.0"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:imports")

val munit = "org.scalameta" %% "munit" % "1.0.2" % Test

lazy val kernel = project.in(file("kernel"))
  .settings(libraryDependencies += munit)

// MIGRATION-PLAN.md Phase 1: the first System split slice. Pure effect
// contracts (system-interface) vs. their concrete, effectful implementations
// (system-handler) — see docs/architecture.md.
lazy val systemInterface = project.in(file("system-interface"))
  .dependsOn(kernel)
  .settings(libraryDependencies += munit)

lazy val systemHandler = project.in(file("system-handler"))
  .dependsOn(kernel, systemInterface)
  .settings(libraryDependencies += munit)

// MIGRATION-PLAN.md Phase 2 (first slice): the untrusted-proposer half of
// the old proof.Proof.scala (Search/Tactics) — depends only on kernel,
// which now also holds the independent Checker they call into.
lazy val core = project.in(file("core"))
  .dependsOn(kernel)
  .settings(libraryDependencies += munit)

lazy val workbench = project.in(file("workbench"))
  .dependsOn(kernel)
  .settings(libraryDependencies += munit)

lazy val proof = project.in(file("proof"))
  .dependsOn(workbench)
  .settings(libraryDependencies += munit)

lazy val compute = project.in(file("compute"))
  .dependsOn(workbench)
  .settings(libraryDependencies += munit)

lazy val rosetta = project.in(file("rosetta"))
  .dependsOn(proof, compute, core)
  .settings(libraryDependencies += munit)

lazy val ledger = project.in(file("ledger"))
  .dependsOn(rosetta, systemInterface, systemHandler)
  .settings(libraryDependencies += munit)

lazy val surface = project.in(file("surface"))
  .dependsOn(ledger)
  .settings(libraryDependencies += munit)

// Domain packs live outside the kernel layers (§4.11); they may use any layer.
lazy val examples = project.in(file("examples"))
  .dependsOn(surface)
  .settings(libraryDependencies += munit)

// Cross-layer acceptance suites (one per phase).
lazy val tests = project.in(file("tests"))
  .dependsOn(examples)
  .settings(libraryDependencies += munit)

lazy val root = project.in(file("."))
  .aggregate(kernel, core, systemInterface, systemHandler, workbench, proof, compute, rosetta, ledger, surface, examples, tests)
  .settings(publish / skip := true)
