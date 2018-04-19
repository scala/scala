package scala.collection

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit
import scala.util.Try

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ToShortBenchmark {

  @Param(Array("", "1", "100", "10000", "32768"))
  var parsee: String = _

  @Benchmark def parseShort(bh: Blackhole): Unit = {
    bh.consume(parsee.parseShort)
  }

  @Benchmark def toShort(bh: Blackhole): Unit = {
    bh.consume(parsee.toShort)
  }

  @Benchmark def checkedToShort(bh: Blackhole): Unit = {
    try {
      val b = parsee.toShort
      bh.consume(b)
    } catch {
      case e: NumberFormatException => bh.consume(e)
    }
  }

}
