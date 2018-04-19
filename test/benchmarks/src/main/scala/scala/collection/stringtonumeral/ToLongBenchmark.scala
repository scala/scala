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
@Warmup(iterations = 4)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ToLongBenchmark {

  @Param(Array("", "1", "1000000", "1000000000000", "9223372036854775807", "9223372036854775808"))
  var parsee: String = _

  @Benchmark def parseLong(bh: Blackhole): Unit = {
    bh.consume(parsee.parseLong)
  }

  @Benchmark def toLong(bh: Blackhole): Unit = {
    bh.consume(parsee.toLong)
  }

  @Benchmark def checkedToLong(bh: Blackhole): Unit = {
    try {
      val b = parsee.toLong
      bh.consume(b)
    } catch {
      case e: NumberFormatException => bh.consume(e)
    }
  }

}
