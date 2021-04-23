package scala.reflect.internal

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit

import scala.reflect.internal.util.BatchSourceFile

@BenchmarkMode(Array(org.openjdk.jmh.annotations.Mode.SampleTime))
@Fork(4)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class LubBenchmark {
  import scala.tools.nsc._
  var g: Global = _
  var ts: List[Global#Type] = _

  trait A1 ; trait A2 ; trait A3 extends A1 ; trait A4 extends A2 ; trait A5 ; trait A6 ; trait A7 ; trait A8 extends A7

  trait Odd extends A1 with A3 with A5 with A7
  trait Even extends A2 with A3 with A6 with A8
  trait Low extends A1 with A2 with A3 with A4
  trait High extends A5 with A6 with A7 with A8
  trait All extends A1 with A2 with A3 with A4 with A5 with A6 with A7 with A8
  class B1 extends A1 with A2
  class B2 extends A7 with A8
  class B3 extends B2 with Low
  class B4 extends B1 with High

  @Setup(Level.Trial)
  def setup(): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    val global = new Global(settings)
    g = global
    val run = new global.Run()
    import language.existentials
    val tp = global.typeOf[((A1, A2, A3, A4), (Odd, Even, High, Low), (B1, B2, B3, B4) )]
    ts = tp.typeArgs
  }

  @Benchmark def measure(bh: Blackhole): Any = {
    val global = g
    import global._
    lub(ts.asInstanceOf[List[Type]])
  }
}
