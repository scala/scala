package strawman.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import strawman.collection.{ArrayView, View}
import scala.{Any, AnyRef, Int, Long, Unit, Array}
import scala.Predef.intWrapper

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(15)
@Warmup(iterations = 30)
@Measurement(iterations = 15)
@State(Scope.Benchmark)
class ArrayBaselineBenchmark {

  @Param(scala.Array("39", "282", "73121", "7312102"))
  var size: Int = _

  @Param(scala.Array("39"))
  var vLoSize: Int = _

  var shortRangingFactor : Int = (size * 0.2).toInt

  var v: Array[Long] = _
  var vLo: Array[Long] = _

  @Setup(Level.Trial)
  def initTrial(): Unit = {

    def fillArray(range: Int) = {
      val array = new Array[Long](range)
      var i = 0
      while (i < range) {
        array(i) = scala.util.Random.nextInt(size).toLong
        i += 1
      }
      array
    }

    v = fillArray(size)
    vLo = fillArray(vLoSize)
  }

  @Benchmark
  def sum(bh: Blackhole) = {
    var i = 0
    var ret = 0L
    while (i < v.length) {
      ret += v(i)
      i += 1
    }
    bh.consume(ret)
  }

  @Benchmark
  def sumOfSquares(bh: Blackhole) = {
    var i = 0
    var ret = 0L
    while (i < v.length) {
      ret += v(i) * v(i)
      i += 1
    }
    bh.consume(ret)
  }

  @Benchmark
  def sumOfSquaresEven(bh: Blackhole) = {
    var i = 0
    var ret = 0L
    while (i < v.length) {
      if (v(i) % 2L == 0L)
        ret += v(i) * v(i)
      i += 1
    }
    bh.consume(ret)
  }

  @Benchmark
  def maps(bh: Blackhole)= {
    var i=0
    var ret=0L
    while (i < v.length) {
      ret += v(i) +
        (v(i) & 0xD) + 0xCAFED00D +
        (v(i) & 0xE) + 0xD15EA5E +
        (v(i) & 0xA) + 0xDABBAD00
      i += 1
    }
    bh.consume(ret)
  }

  @Benchmark
  def filters(bh: Blackhole) = {
    var i=0
    var ret=0L
    while (i < v.length) {
      if ((v(i) & 0x13) != 0x11 &&
          (v(i) & 0x12) == 0x12 &&
          (v(i) & 0x11) != 0x10)
        ret += v(i)
      i += 1
    }
    bh.consume(ret)
  }

  @Benchmark
  def cart(bh: Blackhole)= {
    var d, dp = 0
    var ret = 0L
    while (d < v.length) {
      dp = 0
      while (dp < vLo.length) {
        ret += v(d) * vLo(dp)
        dp += 1
      }
      d += 1
    }
    bh.consume(ret)
  }

  @Benchmark
  def flatMap_take(bh: Blackhole) = {
    var counter1 = 0
    var counter2 = 0
    var ret = 0L
    var n = 0
    var flag = true
    val size1 = v.length
    val size2 = vLo.length
    while (counter1 < size1 && flag) {
      val item1 = v(counter1)
      while (counter2 < size2 && flag) {
        val item2 = vLo(counter2)
        ret = ret + item1 * item2
        counter2 += 1
        n += 1
        if (n == shortRangingFactor)
          flag = false
      }
      counter2 = 0
      counter1 += 1
    }
    bh.consume(ret)
  }
}
