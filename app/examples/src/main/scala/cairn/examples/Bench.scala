package cairn.examples

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc
import cairn.examples.icnet.IcNet

/** M50: benchmark harness — honest numbers for compiled vs interpretive rule
  * dispatch, packrat parsing, and parallel vs sequential net reduction.
  * Run: sbt "examples/runMain cairn.examples.Bench"
  */
@main def Bench(): Unit =
  def time[A](label: String, warmup: Int = 2, reps: Int = 5)(body: => A): A =
    var result: A = null.asInstanceOf[A]
    for _ <- 1 to warmup do result = body
    val times = (1 to reps).map { _ =>
      val t0 = System.nanoTime()
      result = body
      (System.nanoTime() - t0) / 1e6
    }
    println(f"$label%-46s ${times.min}%9.2f ms (best of $reps)")
    result

  val lang = Stlc.language
  val rnd = new scala.util.Random(42)
  def gen(d: Int, env: List[String]): Cst =
    rnd.nextInt(if d <= 0 then 2 else 5) match
      case 0 => Stlc.tru
      case 1 => Stlc.fls
      case 2 if env.nonEmpty => Stlc.v(env(rnd.nextInt(env.length)))
      case 2 => Stlc.fls
      case 3 =>
        val x = s"v${env.length}"
        Stlc.app1(Stlc.lam1(x, Stlc.tBool, gen(d - 1, x :: env)), gen(d - 1, env))
      case _ => Cst.node("if", gen(d - 1, env), gen(d - 1, env), gen(d - 1, env))
  val corpus = List.fill(400)(gen(7, Nil))

  println("== rule dispatch (400 terms, depth 7) ==")
  time("interpretive TreeEngine")(corpus.foreach(t => TreeEngine.normalize(lang, t, fuel = 100_000)))
  val compiled = CompiledTreeEngine(lang)
  time("compiled dispatch (M28)")(corpus.foreach(t => compiled.normalize(t, fuel = 100_000)))

  println("== parsing (packrat, M10) ==")
  val bigSource = (1 to 200).map(i => s"fun v$i : Bool . ").mkString + "true"
  time("parse 200-lambda chain")(cairn.core.Parser.parse(lang.grammar, bigSource))

  println("== net reduction (M27) ==")
  val two = Stlc.lam1("f", Stlc.tBool,
    Stlc.lam1("x", Stlc.tBool, Stlc.app1(Stlc.v("f"), Stlc.app1(Stlc.v("f"), Stlc.v("x")))))
  val netTerm = Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.tru)
  val (net, _) = IcNet.lower(netTerm).toOption.get
  time("sequential net reduction")(NetEngine.normalize(IcNet.language, net))
  val (_, stats) = time("parallel net reduction")(NetEngine.normalizeParallel(IcNet.language, net)).toOption.get
  println(s"  parallel sweeps: ${stats.sweeps}, pairs/sweep: ${stats.pairsPerSweep.mkString(",")}")

  println("== genuine concurrent net reduction (real threads, not just fewer sweeps) ==")
  // Honest benchmark, not a cherry-picked one: a net wide enough for
  // parallelism to have SOMETHING to parallelize (200 fully independent
  // dup-dup pairs, no wires between them at all — the case
  // NetEngine.normalizeConcurrent's isolation check allows onto real threads)
  // rather than the tiny 2-3-agent nets elsewhere in this file, where JVM
  // thread/Future overhead would trivially swamp any win.
  given ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  val wideBuilder = NetBuilder()
  for _ <- 1 to 200 do
    val d1 = wideBuilder.agent("dup"); val d2 = wideBuilder.agent("dup")
    val f1 = wideBuilder.agent("free"); val f2 = wideBuilder.agent("free"); val f3 = wideBuilder.agent("free")
    wideBuilder.wire(PortRef(d1, 0), PortRef(d2, 0))
    wideBuilder.wire(PortRef(d1, 1), PortRef(f1, 0))
    wideBuilder.wire(PortRef(d2, 1), PortRef(f2, 0)); wideBuilder.wire(PortRef(d2, 2), PortRef(f3, 0))
    wideBuilder.wire(PortRef(d1, 2), PortRef(wideBuilder.agent("free"), 0))
  val wideNet = wideBuilder.net
  time("sequential (200 independent pairs)")(NetEngine.normalize(IcNet.language, wideNet))
  time("parallelStep sweeps (200 independent pairs)")(NetEngine.normalizeParallel(IcNet.language, wideNet))
  val (_, _, threadedCount) =
    time("genuine concurrent, real threads (200 independent pairs)")(
      NetEngine.normalizeConcurrent(IcNet.language, wideNet)).toOption.get
  println(s"  pairs actually dispatched to real threads (vs. sequential fallback): $threadedCount")
