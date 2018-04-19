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
class ToDoubleBenchmark {

  @Param(Array("", "1", "100.00E458", "0x4a5568ab.ap2", "100000000000000000000.00E18", "not a double"))
  var parsee: String = _

  @Benchmark def parseDouble(bh: Blackhole): Unit = {
    bh.consume(parsee.parseDouble)
  }

  @Benchmark def toDouble(bh: Blackhole): Unit = {
    bh.consume(parsee.toDouble)
  }

  @Benchmark def checkedToDouble(bh: Blackhole): Unit = {
    try {
      val b = parsee.toDouble
      bh.consume(b)
    } catch {
      case e: NumberFormatException => bh.consume(e)
    }
  }

}
