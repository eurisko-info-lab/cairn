package cairn.core

import cairn.kernel.*

/** Pure host-project scaffold planning (MIGRATION-PLAN.md Phase 2, tenth
  * slice — the optional `Scaffold.plan` revisit). Computes every project
  * record and every `(relativePath, content)` write with zero I/O and no
  * `java.nio.file.Path` — only `/`-joined relative strings under an implicit
  * emit root. `rosetta.Scaffold` remains the thin executor: resolve against a
  * root `Path` and write via `system-handler.Filesystem`.
  */
object ScaffoldPlan:
  final case class Project(
      host: String,
      root: String,
      mainFile: String,
      buildCommand: List[String])

  private def join(parts: String*): String =
    parts.filter(_.nonEmpty).mkString("/")

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

  /** Pure planning: every `Project` record and every `(relPath, content)` pair
    * an emitter needs to write — no filesystem access, no absolute paths.
    */
  def plan(m: RosettaModule2): Either[String, (List[Project], List[(String, String)])] =
    val ports = List(Ports2.ScalaPort2, Ports2.LeanPort2, Ports2.HaskellPort2, Ports2.RustPort2)
    val zero: Either[String, (List[Project], List[(String, String)])] = Right((Nil, Nil))
    val built = ports.foldLeft(zero) { (acc, port) =>
      acc.flatMap { (ps, writes) =>
        PortV2.verified(port, m).map { out =>
          val root = port.hostName
          val (mainFile, buildCmd, newWrites) = port.hostName match
            case "scala" =>
              val f = join(root, out.fileName)
              (f, List("scala-cli", "run", "--server=false", f), List(f -> out.text))
            case "lean" =>
              val f = join(root, out.fileName)
              val lakefile = join(root, "lakefile.lean") -> s"""import Lake
                   |open Lake DSL
                   |package ${m.name} where
                   |lean_lib ${m.name.capitalize} where
                   |""".stripMargin
              (f, List("lake", "build"), List(f -> out.text, lakefile))
            case "haskell" =>
              val f = join(root, out.fileName)
              (f, List("runghc", f), List(f -> out.text))
            case "rust" =>
              val f = join(root, "src", "main.rs")
              val cargoToml = join(root, "Cargo.toml") -> s"""[package]
                   |name = "${m.name.replace('-', '_')}"
                   |version = "0.1.0"
                   |edition = "2021"
                   |""".stripMargin
              (f, List("cargo", "test", "--quiet"), List(f -> out.text, cargoToml))
            case _ => (join(root, out.fileName), Nil, Nil)
          (ps :+ Project(port.hostName, root, mainFile, buildCmd), writes ++ newWrites)
        }
      }
    }
    built.flatMap { case (ps, writes) =>
      obligationsManifest(m, ports.map(p => p.emit(m).toOption.get)).map { manifest =>
        (ps, writes :+ ("obligations.json" -> manifest))
      }
    }
