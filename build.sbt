ThisBuild / scalaVersion := "3.3.4"
ThisBuild / organization := "cairn"
ThisBuild / version := "0.1.0"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:imports")

val munit = "org.scalameta" %% "munit" % "1.0.2" % Test

lazy val kernel = project.in(file("kernel"))
  .settings(libraryDependencies += munit)

// MIGRATION-PLAN.md Phase 1: System Interface vs Handler.
lazy val systemInterface = project.in(file("system-interface"))
  .dependsOn(kernel)
  .settings(libraryDependencies += munit)

lazy val systemHandler = project.in(file("system-handler"))
  .dependsOn(kernel, core, systemInterface)
  .settings(libraryDependencies += munit)

// MIGRATION-PLAN.md Phase 2: pure proposal machinery.
lazy val core = project.in(file("core"))
  .dependsOn(kernel)
  .settings(libraryDependencies += munit)

// MIGRATION-PLAN.md Phase 6: User — languages/policies/workflows; never imports handlers.
lazy val user = project.in(file("user"))
  .dependsOn(kernel, core, systemInterface)
  .settings(libraryDependencies += munit)

// MIGRATION-PLAN.md Phase 8: composition root (PackLoader, CLI wiring).
lazy val runtime = project.in(file("runtime"))
  .dependsOn(user, systemHandler, core, kernel, systemInterface)
  .settings(libraryDependencies += munit)

lazy val proof = project.in(file("proof"))
  .dependsOn(core, kernel)
  .settings(libraryDependencies += munit)

lazy val rosetta = project.in(file("rosetta"))
  .dependsOn(proof, core, systemHandler)
  .settings(libraryDependencies += munit)

lazy val surface = project.in(file("surface"))
  .dependsOn(proof, runtime, systemHandler)
  .settings(libraryDependencies += munit)

// Demos / entrypoints — may use the full stack (runtime composition).
lazy val examples = project.in(file("examples"))
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

lazy val tests = project.in(file("tests"))
  .dependsOn(examples, rosetta, runtime, user)
  .settings(libraryDependencies += munit)

lazy val root = project.in(file("."))
  .aggregate(
    kernel, core, systemInterface, systemHandler, user, runtime,
    proof, rosetta, surface, examples, tests)
  .settings(publish / skip := true)
