package scala.reflect.internal.tpe

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.tools.nsc.{Global, Settings}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class TypeComparersBenchmark {
  // Capacity hint `knownFalseSubTypes` has been grown to by an earlier, deeper subtype check.
  // `IdentityHashMap.clear` never shrinks the table, so each top-level subtype check pays for it.
  @Param(Array("0", "1024"))
  var grownTo: Int = _

  var g: Global = _
  var tp1, tp2: Global#Type = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    val global = new Global(settings)
    g = global
    new global.Run()
    tp1 = global.definitions.StringTpe
    tp2 = global.definitions.AnyRefTpe
    val m = global.knownFalseSubTypes
    for (i <- 0 until grownTo) m.put(global.ConstantType(global.Constant(i)), Nil)
    m.clear()
  }

  @Benchmark def isSubType(): Boolean = {
    val global = g
    import global._
    tp1.asInstanceOf[Type] <:< tp2.asInstanceOf[Type]
  }
}
