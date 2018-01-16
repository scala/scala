package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

object ListBenchmark {
  case class Content(value: Int)
}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ListBenchmark {
  import ListBenchmark._
  @Param(Array("0", "1", "10", "100", "1000"))
  var size: Int = _

  var values: List[Content] = _
  var mid: Content = _
  var last: Content = _
  var replacement: Content = _


  @Setup(Level.Trial) def initKeys(): Unit = {
    values = List.tabulate(size)(v => Content(v))
    mid = Content(size / 2)
    last = Content(Math.max(0,size -1))
    replacement = Content(size * 2 + 1)
  }

  @Benchmark def filter_includeAll: Any = {
    values.filter(v => true)
  }

  @Benchmark def filter_excludeAll: Any = {
    values.filter(_ => false)
  }

  @Benchmark def filter_exc_mid: Any = {
    values.filter(v => v.value != mid.value)
  }

  @Benchmark def filter_from_mid: Any = {
    values.filter(v => v.value <= mid.value)
  }

  @Benchmark def filter_exc_last: Any = {
    values.filter(v => v.value != last.value)
  }

  @Benchmark def filter_only_last: Any = {
    values.filter(v => v.value == last.value)
  }

  @Benchmark def mapConserve_identity: Any = {
    values.mapConserve(x => x)
  }

  @Benchmark def mapConserve_modifyAll: Any = {
    values.mapConserve(x => replacement)
  }

  @Benchmark def mapConserve_modifyMid: Any = {
    values.mapConserve(x => if (x == mid) replacement else x)
  }

  @Benchmark def concat_raw: Any = {
    List.concat(values, values)
  }

  @Benchmark def fill_raw: Any = {
    List.fill(size)(last)
  }

  @Benchmark def tabulate_raw: Any = {
    List.tabulate(size)(values.apply)
  }

  @Benchmark def range_raw: Any = {
    List.range(-size, size, 2)
  }

  @Benchmark def iterate_raw: Any = {
    List.iterate(last, size)(v => v.copy(value = v.value + 10))
  }
}
