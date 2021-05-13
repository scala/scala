package scala.math

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BigIntEulerProblem15Benchmark {

  @Param(Array("5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85", "90", "95", "100"))
  var size: Int = _

  @Benchmark
  def eulerProblem15(bh: Blackhole): Unit = {
    def f(row: Array[BigInt], c: Int): BigInt =
      if (c == 0) row.last else f(row.scan(BigInt(0))(_ + _), c - 1)
    def computeAnswer(n: Int): BigInt = f(Array.fill(n + 1)(BigInt(1)), n)
    bh.consume(computeAnswer(size))
  }

}
