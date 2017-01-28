package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ListBenchmark {
  @Param(Array("0", "10", "100", "1000"))
  var size: Int = _

  var values: List[String] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    values = List.tabulate(size)(n => if (n == size / 2) "mid" else "")
  }

  @Benchmark def mapConserve_identity: Any = {
    values.mapConserve(x => x)
  }

  @Benchmark def mapConserve_modifyAll: Any = {
    values.mapConserve(x => "replace")
  }
  @Benchmark def mapConserve_modifyMid: Any = {
    values.mapConserve(x => if (x == "mid") "replace" else x)
  }
}
