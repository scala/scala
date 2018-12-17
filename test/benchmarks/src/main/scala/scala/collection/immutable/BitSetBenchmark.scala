package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 6)
@Measurement(iterations = 6)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BitSetBenchmark {
  @Param(Array("1", "1000", "10000000"))
  var sizeLeft: Int = _

  var percentageFull: Double = 0.3

  var bitSet: BitSet = _
  var arg: BitSet = _

  @Setup(Level.Iteration) def initNumbers: Unit = {
    bitSet = (0 to sizeLeft).filter(_ => Random.nextDouble() <= percentageFull).to(BitSet)
  }

  @Benchmark
  def filter(bh: Blackhole): Unit = {
    (1 to 10) foreach { _ =>
      bh.consume(bitSet.filter(_ % 2 == 0))
    }
  }
}
