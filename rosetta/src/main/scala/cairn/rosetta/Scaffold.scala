package cairn.rosetta

import cairn.kernel.*
import cairn.workbench.JsonSurface
import cairn.systemhandler.Filesystem
import java.nio.file.Path

/** M33: full host project scaffolds + machine-readable obligations manifest
  * linking every theorem to its host artifact. The manifest is a Cst encoded
  * through the JSON surface (M12) — no ad-hoc JSON strings.
  *
  * MIGRATION-PLAN.md Phase 2 (fourth slice): `plan` is the pure half —
  * computes every `Project` record and every file to write, without
  * touching disk. `emitAll` is now a thin execution wrapper over
  * `system-handler.Filesystem`. `plan` stays here rather than moving to
  * `core`: `obligationsManifest` needs `cairn.workbench.JsonSurface`, and
  * `core` cannot depend on `workbench` (the dependency runs the other way) —
  * moving it would cycle. A future slice could revisit this once
  * `JsonSurface` (confirmed I/O-free) has its own Core/Kernel-reachable home.
  *
  * One deliberate behavior refinement from the pre-split version: writes now
  * happen only after the full plan succeeds, instead of incrementally as
  * each host's port is verified — so a later port failing no longer leaves
  * earlier hosts' files partially written. Nothing tests or relies on the
  * old partial-write-on-failure behavior.
  */
object Scaffold:
  final case class Project(host: String, root: Path, mainFile: Path, buildCommand: List[String])

  def obligationsManifest(m: RosettaModule2, outputs: List[PortOutput]): Either[String, String] =
    val entries = for
      t <- m.theorems
      o <- outputs
    yield Cst.node("entry",
      Cst.node("theorem", Cst.Leaf(t.name)),
      Cst.node("host", Cst.Leaf(o.hostName)),
      Cst.node("file", Cst.Leaf(o.fileName)),
      Cst.node("artifact", Cst.Leaf(m.artifact.digest.hex)))
    JsonSurface.encode(Cst.node("obligations", Cst.Node("list", entries)))

  /** Pure planning: every `Project` record and every `(path, content)` pair
    * `emitAll` needs to write — no filesystem access.
    */
  private def plan(m: RosettaModule2, dir: Path): Either[String, (List[Project], List[(Path, String)])] =
    val ports = List(Ports2.ScalaPort2, Ports2.LeanPort2, Ports2.HaskellPort2, Ports2.RustPort2)
    val zero: Either[String, (List[Project], List[(Path, String)])] = Right((Nil, Nil))
    val built = ports.foldLeft(zero) { (acc, port) =>
      acc.flatMap { (ps, writes) =>
        PortV2.verified(port, m).map { out =>
          val root = dir.resolve(port.hostName)
          val (mainFile, buildCmd, newWrites) = port.hostName match
            case "scala" =>
              val f = root.resolve(out.fileName)
              (f, List("scala-cli", "run", "--server=false", f.toString), List(f -> out.text))
            case "lean" =>
              val f = root.resolve(out.fileName)
              val lakefile = root.resolve("lakefile.lean") -> s"""import Lake
                   |open Lake DSL
                   |package ${m.name} where
                   |lean_lib ${m.name.capitalize} where
                   |""".stripMargin
              (f, List("lake", "build"), List(f -> out.text, lakefile))
            case "haskell" =>
              val f = root.resolve(out.fileName)
              (f, List("runghc", f.toString), List(f -> out.text))
            case "rust" =>
              val src = root.resolve("src")
              val f = src.resolve("main.rs")
              val cargoToml = root.resolve("Cargo.toml") -> s"""[package]
                   |name = "${m.name.replace('-', '_')}"
                   |version = "0.1.0"
                   |edition = "2021"
                   |""".stripMargin
              (f, List("cargo", "test", "--quiet"), List(f -> out.text, cargoToml))
            case other => (root.resolve(out.fileName), Nil, Nil)
          (ps :+ Project(port.hostName, root, mainFile, buildCmd), writes ++ newWrites)
        }
      }
    }
    built.flatMap { case (ps, writes) =>
      obligationsManifest(m, ports.map(p => p.emit(m).toOption.get)).map { manifest =>
        (ps, writes :+ (dir.resolve("obligations.json") -> manifest))
      }
    }

  def emitAll(m: RosettaModule2, dir: Path): Either[String, List[Project]] =
    plan(m, dir).map { (ps, writes) =>
      writes.foreach((p, content) => Filesystem.writeFile(p, content))
      ps
    }
