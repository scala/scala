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
class ArrayViewBenchmark {

  @Param(scala.Array("39", "282", "73121", "7312102"))
  var size: Int = _

  @Param(scala.Array("39"))
  var vLoSize: Int = _

  var shortRangingFactor : Int = (size * 0.2).toInt

  var v: View[Long] = _
  var vLo: View[Long] = _

  def fillArray(range: Int) = {
    val array = new Array[Long](range)
    var i = 0
    while (i < range) {
      array(i) = scala.util.Random.nextInt(size).toLong
      i += 1
    }
    array
  }

  @Setup(Level.Trial)
  def initData(): Unit = {

    v = ArrayView(fillArray(size))
    vLo = ArrayView(fillArray(vLoSize))
  }

  @Benchmark
  def sum (bh: Blackhole) = {
    val ret : Long = v.foldLeft(0L)(_+_)
    bh.consume(ret)
  }

  @Benchmark
  def sumOfSquares (bh: Blackhole) = {
    val ret : Long = v
      .map(d => d * d)
      .foldLeft(0L)(_+_)
    bh.consume(ret)
  }

  @Benchmark
  def sumOfSquaresEven (bh: Blackhole) = {
    val ret : Long = v
      .filter(x  => x % 2L == 0L)
      .map(x => x * x)
      .foldLeft(0L)(_+_)
    bh.consume(ret)
  }

  @Benchmark
  def maps (bh: Blackhole) = {
    val ret : Long = v
      .map(x => x + (x & 0xD) + 0xCAFED00D)
      .map(x => x + (x & 0xE) + 0xD15EA5E)
      .map(x => x + (x & 0xA) + 0xDABBAD00)
      .foldLeft(0L)(_+_)
    bh.consume(ret)
  }

  @Benchmark
  def filters (bh: Blackhole) = {
    val ret : Long = v
      .filter(x => (x & 0x13) != 0x11)
      .filter(x => (x & 0x12) == 0x12)
      .filter(x => (x & 0x11) != 0x10)
      .foldLeft(0L)(_+_)
    bh.consume(ret)
  }

  @Benchmark
  def cart (bh: Blackhole) = {
    val ret : Long = v
      .flatMap(d => vLo.map (dp => dp * d))
      .foldLeft(0L)(_+_)
    bh.consume(ret)
  }

  @Benchmark
  def flatMap_take (bh: Blackhole) = {
    val ret = v
      .flatMap((x) => vLo
        .map((dP) => dP * x))
      .take(shortRangingFactor)
      .foldLeft(0L)(_+_)
    bh.consume(ret)
  }
}
