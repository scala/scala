package scala.math

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.annotation.tailrec

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BigIntFactorialBenchmark {

  @Param(Array("5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85", "90", "95", "100"))
  var size: Int = _

  @Benchmark
  def factorial(bh: Blackhole): Unit = {
    @tailrec def fact(i: Int, n: Int, prev: BigInt): BigInt =
      if (i > n) prev else fact(i + 1, n, prev * i)
    bh.consume(fact(1, size, BigInt(1)))
  }

}
