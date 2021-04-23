package scala.reflect.internal

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit

import scala.reflect.internal.util.BatchSourceFile

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class SymbolBenchmark {
  import scala.tools.nsc._
  var g: Global = _
  var symbol: Global#Symbol = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    settings.stopAfter.value = List("typer")
    val global = new Global(settings)
    g = global

    val run = new global.Run()
    val source = g.newSourceFile("package p1; class C { def foo: List[String] = Nil }")
    run.compileSources(source :: Nil)
    val foo = global.rootMirror.getClassIfDefined("p1.C").info.decl(global.newTermName("foo"))
    symbol = foo
  }

  @Benchmark def measure(bh: Blackhole): Unit = {
    val r = g.currentRun
    g.phase = r.erasurePhase
    bh.consume(symbol.info)
    g.phase = r.typerPhase
    bh.consume(symbol.info)

  }
}
