package scala.collection.immutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class VectorIterationBenchmark {
  @Param(Array("0", "1", "2", "3", "4", "6", "8", "10", "12", "14", "16"
    , "20", "24", "28", "32", "40", "48", "56", "64",
    "128", "256", "512", "1024", "2048", "4096", "8192", "16384", "32768", "65536"))
  var size: Int = _

  var value: Vector[Any] = _
  var larger: Vector[Any] = _

  @Setup(Level.Trial) def init(): Unit = {
    value = Vector.tabulate(size)(_.toString)
    larger = value :+ "last"
  }

  @Benchmark def iterate(bh: Blackhole) = {
    var i = 0
    val it = value.iterator
    while (i < size) {
      bh.consume(it.next)
      i += 1
    }
  }

  @Benchmark def apply(bh: Blackhole) = {
    var i = 0
    while (i < size) {
      bh.consume(value(i))
      i += 1
    }
  }

  @Benchmark def applySingle(bh: Blackhole) = {
    bh.consume(larger(size))
  }

  @Benchmark def drop = {
    val it = value.iterator
    it.drop(size)
  }
}
