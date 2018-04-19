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
@Warmup(iterations = 15)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ToIntBenchmark {

  @Param(Array("", "1", "10000", "2147483647", "2147483648"))
  var parsee: String = _

  @Benchmark def parseInt(bh: Blackhole): Unit = {
    bh.consume(parsee.parseInt)
  }

  @Benchmark def toInt(bh: Blackhole): Unit = {
    bh.consume(parsee.toInt)
  }

  @Benchmark def checkedToInt(bh: Blackhole): Unit = {
    try {
      val b = parsee.toInt
      bh.consume(b)
    } catch {
      case e: NumberFormatException => bh.consume(e)
    }
  }

}
