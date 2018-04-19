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
class ToBoolBenchmark {

  @Param(Array("", "true", "false", "TruE", "not true"))
  var parsee: String = _

  @Benchmark def parseBool(bh: Blackhole): Unit = {
    bh.consume(parsee.parseBoolean)
  }

  @Benchmark def toBool(bh: Blackhole): Unit = {
    bh.consume(parsee.toBoolean)
  }

  @Benchmark def checkedtoBool(bh: Blackhole): Unit = {
    try {
      val b = parsee.toBoolean
      bh.consume(b)
    } catch {
      case e: NumberFormatException => bh.consume(e)
    }
  }

}
