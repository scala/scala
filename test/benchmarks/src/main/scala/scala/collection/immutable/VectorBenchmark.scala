package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.infra.Blackhole

import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class VectorBenchmark {
  @Param(Array("0", "10", "1000", "1000000"))
  var size: Int = _
  var vec: Vector[AnyRef] = _
  val array = Array.fill(1000000)(new AnyRef)

  @Setup(Level.Trial) def initKeys(): Unit = {
    vec = Vector.fill(size)(new AnyRef)
  }
  @Benchmark def concat(bh: Blackhole): Any = {
    bh.consume(vec.copyToArray(array, 0, size))
  }
}
