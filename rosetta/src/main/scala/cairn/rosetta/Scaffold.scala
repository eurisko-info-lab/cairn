package cairn.rosetta

import cairn.kernel.*
import cairn.workbench.JsonSurface
import java.nio.file.{Files, Path}

/** M33: full host project scaffolds + machine-readable obligations manifest
  * linking every theorem to its host artifact. The manifest is a Cst encoded
  * through the JSON surface (M12) — no ad-hoc JSON strings.
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

  def emitAll(m: RosettaModule2, dir: Path): Either[String, List[Project]] =
    val ports = List(Ports2.ScalaPort2, Ports2.LeanPort2, Ports2.HaskellPort2, Ports2.RustPort2)
    val zero: Either[String, List[Project]] = Right(Nil)
    val projects = ports.foldLeft(zero) { (acc, port) =>
      acc.flatMap { ps =>
        PortV2.verified(port, m).map { out =>
          val root = dir.resolve(port.hostName)
          Files.createDirectories(root)
          val (mainFile, buildCmd) = port.hostName match
            case "scala" =>
              val f = root.resolve(out.fileName)
              Files.writeString(f, out.text)
              (f, List("scala-cli", "run", "--server=false", f.toString))
            case "lean" =>
              val f = root.resolve(out.fileName)
              Files.writeString(f, out.text)
              Files.writeString(root.resolve("lakefile.lean"),
                s"""import Lake
                   |open Lake DSL
                   |package ${m.name} where
                   |lean_lib ${m.name.capitalize} where
                   |""".stripMargin)
              (f, List("lake", "build"))
            case "haskell" =>
              val f = root.resolve(out.fileName)
              Files.writeString(f, out.text)
              (f, List("runghc", f.toString))
            case "rust" =>
              val src = root.resolve("src")
              Files.createDirectories(src)
              val f = src.resolve("main.rs")
              Files.writeString(f, out.text)
              Files.writeString(root.resolve("Cargo.toml"),
                s"""[package]
                   |name = "${m.name.replace('-', '_')}"
                   |version = "0.1.0"
                   |edition = "2021"
                   |""".stripMargin)
              (f, List("cargo", "test", "--quiet"))
            case other => (root.resolve(out.fileName), Nil)
          ps :+ Project(port.hostName, root, mainFile, buildCmd)
        }
      }
    }
    for
      ps <- projects
      manifest <- obligationsManifest(m,
        ports.map(p => p.emit(m).toOption.get))
      _ = Files.writeString(dir.resolve("obligations.json"), manifest)
    yield ps
