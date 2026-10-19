package scala.reflect.internal

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.reflect.internal.util.BatchSourceFile

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 8, time = 1)
@Measurement(iterations = 8, time = 1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class LubRefinementBenchmark {
  import scala.tools.nsc._

  @Param(Array("arrayseq", "collections", "wide", "smallbase"))
  var example: String = _

  var g: Global = _
  var ts: List[Global#Type] = _

  // A trait with many members, none of which are overridden in the subclasses,
  // so the lub of the subclasses is just `Wide`. Unless the members are pruned by name,
  // every member of `Wide` is collected and checked for being refined in each subclass.
  private def wideSource: String = {
    val traits = (1 to 20).map(i => s"trait W$i { ${(1 to 50).map(j => s"def m${i}_$j(x: Int): Int = x").mkString("; ")} }")
    val wide = s"trait Wide extends ${(1 to 20).map("W" + _).mkString(" with ")}"
    val subs = (1 to 10).map(i => s"class C$i extends Wide { def own$i = $i }")
    (Seq("package synth") ++ traits ++ Seq(wide) ++ subs).mkString("\n")
  }

  @Setup(Level.Trial)
  def setup(): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    settings.stopAfter.value = List("typer")
    val global = new Global(settings)
    g = global
    import global._
    val run = new Run()
    run.compileSources(List(new BatchSourceFile("synth.scala", wideSource)))
    assert(!reporter.hasErrors)
    def cls(name: String): Type = rootMirror.getRequiredClass(name).tpe
    def app(name: String, arg: Type): Type = appliedType(rootMirror.getRequiredClass(name), arg)
    ts = example match {
      case "arrayseq" =>
        // the lub of the branches of `mutable.ArraySeq.make`
        app("scala.collection.mutable.ArraySeq.ofRef", definitions.AnyRefTpe) ::
          List("Int", "Double", "Long", "Float", "Char", "Byte", "Short", "Boolean", "Unit").map(n => cls(s"scala.collection.mutable.ArraySeq.of$n"))
      case "collections" =>
        List("scala.collection.immutable.List", "scala.collection.immutable.Vector", "scala.collection.mutable.ArrayBuffer", "scala.collection.mutable.ListBuffer").map(app(_, definitions.IntTpe))
      case "smallbase" =>
        // a small lub base type (`java.io.Serializable`), with large lubbed types
        List(app("scala.collection.immutable.Vector", definitions.IntTpe), definitions.StringTpe, app("scala.collection.mutable.ArrayBuffer", definitions.IntTpe), cls("scala.math.BigInt"))
      case "wide" =>
        (1 to 10).toList.map(i => cls(s"synth.C$i"))
    }
    global.phase = run.typerPhase
    println(s"\n$example: lub = ${lub(ts.asInstanceOf[List[Type]])}")
  }

  @Benchmark def measure(): Any = {
    val global = g
    import global._
    lub(ts.asInstanceOf[List[Type]])
  }
}
