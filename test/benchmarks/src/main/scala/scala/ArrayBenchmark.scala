package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

object ArrayBenchmark {
  case class Content(value: Int)
}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ArrayBenchmark {
  import ArrayBenchmark._
  @Param(Array("0", "1", "10", "100", "1000"))
  var size: Int = _
  var values: Array[Content] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    values = Array.tabulate(size)(v => Content(v))
  }

  @Benchmark def concat_raw: Any = {
    Array.concat(values, values)
  }

  @Benchmark def fill_raw: Any = {
    Array.fill(size)(Content(0))
  }

  @Benchmark def tabulate_raw: Any = {
    Array.tabulate(size)(values.apply)
  }

  @Benchmark def range_raw: Any = {
    Array.range(-size, size, 2)
  }

  @Benchmark def iterate_raw: Any = {
    Array.iterate(Content(0), size)(v => v.copy(value = v.value + 10))
  }
}
