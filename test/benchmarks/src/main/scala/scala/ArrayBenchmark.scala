package scala

import java.util.concurrent.TimeUnit

import scala.util.Random

import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class ArrayBenchmark {

  // From #1338: Optimize simple for loops

  @Param(Array("10", "100", "1000"))
  var loopSize: Int = _
  var doubles: Array[Array[Double]] = _

  @Setup(Level.Trial) def initArrays: Unit = {
    // doubles = Array.ofDim[Double](loopSize,loopSize)
    // doubles = Array.tabulate(loopSize, loopSize)((i,_) => i.toDouble)
    doubles = Array.tabulate(loopSize, loopSize) { (_, _) =>
      scala.math.sqrt(Random.nextDouble)
    }
  }

  @Benchmark 
  def matMulUsingIteratorsBench(): Unit = {
    matMulUsingIterators(doubles, doubles, doubles)
  }

  def matMulUsingIterators(
    a: Array[Array[Double]],
    b: Array[Array[Double]],
    c: Array[Array[Double]]
  ): Unit = {

    val b_j = new Array[Double](b.length)

    for (j <- 0 until b(0).length) {
      for (k <- 0 until b.length) {
        b_j(k) = b(k)(j)
      }
      for (i <- 0 until a.length) {
        val c_i = c(i)
        val a_i = a(i)
        var s = 0.0d;
        for (k <- 0 until b.length) {
          s += a_i(k) * b_j(k)
        }
        c_i(j) = s
      }
    }
  }

  @Benchmark 
  def matMulUsingRangesBench(): Unit = {
    matMulUsingRanges(doubles, doubles, doubles)
  }

  def matMulUsingRanges(
    a: Array[Array[Double]],
    b: Array[Array[Double]],
    c: Array[Array[Double]]
  ) : Unit = {

    val jRange = 0 until b(0).length;
    val kRange = 0 until b.length;
    val iRange = 0 until a.length;

    val b_j = new Array[Double](b.length)

    for (j <- jRange) {
      for (k <- kRange) {
        b_j(k) = b(k)(j)
      }
      for (i <- iRange) {
        val c_i = c(i);
        val a_i = a(i);
        var s = 0.0d;
        for (k <- kRange) {
          s += a_i(k) * b_j(k)
        }
        c_i(j) = s
      }
    }
  }

  @Benchmark
  def matMulUsingWhileLoopBench(): Unit = {
    matMulUsingWhileLoop(doubles, doubles, doubles)
  }

  def matMulUsingWhileLoop(
    a: Array[Array[Double]],
    b: Array[Array[Double]],
    c: Array[Array[Double]]
  ): Unit = {

    val m = a.length;
    val p = b(0).length;
    val n = b.length;

    val b_j = new Array[Double](b.length);

    var i = 0; var j = 0; var k = 0;
    while (j < p) {
      k = 0
      while (k < n) {
        b_j(k) = b(k)(j);
        k += 1
      }
      i = 0
      while (i < m) {
        val c_i = c(i);
        val a_i = a(i);
        var s = 0.0d;
        k = 0;
        while (k < n) {
          s += a_i(k) * b_j(k);
          k += 1
        }
        c_i(j) = s;
        i += 1
      }
      j += 1;
    }
  }
}
