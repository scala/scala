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
class ListMapBenchmark {
  @Param(Array("1", "10", "100", "1000"))
  var size: Int = _

  var kvs: Iterable[(Int, Int)] = _

  @Setup(Level.Trial)
  def initKeys(): Unit = {
    val unique = (0 to size).map(i => i -> i)
    kvs = unique ++ unique
  }

  @Benchmark
  def builder(bh: Blackhole): Unit = {
    val b = new ListMapBuilder[Int, Int]
    bh.consume(b.addAll(kvs).result())
  }
}
