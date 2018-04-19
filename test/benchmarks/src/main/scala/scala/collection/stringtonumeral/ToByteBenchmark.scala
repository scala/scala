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
class ToByteBenchmark {

  @Param(Array("", "1", "100", "128", "2147483647"))
  var parsee: String = _

  @Benchmark def parseByte(bh: Blackhole): Unit = {
    bh.consume(parsee.parseByte)
  }

  @Benchmark def toByte(bh: Blackhole): Unit = {
    bh.consume(parsee.toByte)
  }

  @Benchmark def checkedToByte(bh: Blackhole): Unit = {
    try {
      val b = parsee.toByte
      bh.consume(b)
    } catch {
      case e: NumberFormatException => bh.consume(e)
    }
  }

}
