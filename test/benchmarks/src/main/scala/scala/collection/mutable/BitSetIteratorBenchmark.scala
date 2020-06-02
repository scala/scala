package scala.collection.mutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.collection.mutable

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 6)
@Measurement(iterations = 6)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BitSetIteratorBenchmark {

  @Param(Array("0", "1", "3", "15", "63", "255"))
  var spacing: Int = _

  var bs: mutable.BitSet = _

  @Setup(Level.Iteration) def initializeRange(): Unit = {
    bs = mutable.BitSet(0 until 1000 by (spacing + 1): _*)
  }

  @Benchmark def iterateAll(): Unit = {
    var sum = 0
    val it = bs.iterator
    while (it.hasNext) sum += it.next()
  }

}
