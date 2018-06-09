package scala.collection

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class LengthCompareOpsBenchmark {
  @Param(Array("0", "1", "10", "100", "1000"))
  var size: Int = _

  @Param(Array("1", "100", "10000"))
  var len: Int = _

  var values: List[Int] = _


  @Setup(Level.Trial) def initKeys(): Unit = {
    values = List.fill(size)(Random.nextInt())
  }

  @Benchmark def lengthCompareUgly: Any = {
    values.lengthCompare(len) == 0
  }

  @Benchmark def lengthComparePretty: Any = {
    values.lengthIs == len
  }
}