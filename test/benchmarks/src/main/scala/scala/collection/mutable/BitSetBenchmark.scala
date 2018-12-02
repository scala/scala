package scala.collection.mutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import java.util.concurrent.TimeUnit

import scala.collection.mutable

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 6)
@Measurement(iterations = 6)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BitSetBenchmark {
  @Param(Array("0", "3", "5", "10", "1000", "1000000"))
  var size: Int = _

  val bitSet = (1 to 1000).to(mutable.BitSet)

  var bs: mutable.BitSet = _

  var range: Range = _

  val clones: Array[mutable.BitSet] = new Array(100)

  @Setup(Level.Iteration) def initializeRange(): Unit = {
    range = (10 to (10 + size))
  }
  @Setup(Level.Invocation) def initializeClones(): Unit = {
    (0 until 100) foreach (i => clones(i) = bitSet.clone())
  }

  @Benchmark def addAll(bh: Blackhole): Unit = {
    clones.foreach{ c =>
      bh consume c.addAll(range)
    }
  }
}
