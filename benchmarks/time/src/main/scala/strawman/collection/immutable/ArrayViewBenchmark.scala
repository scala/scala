package strawman.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import strawman.collection.{ArrayView, View}
import scala.{Any, AnyRef, Int, Long, Unit, Array}
import scala.Predef.intWrapper

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class ArrayViewBenchmark {

  @Param(scala.Array("282", "73121", "7312102"))
  var size: Int = _

  @Param(scala.Array("39"))
  var vLoSize: Int = _

  var shortRangingFactor : Int = (size * 0.2).toInt

  var v: View[Long] = _
  var vLo: View[Long] = _

  @Setup(Level.Trial)
  def initData(): Unit = {

    def fillArray(range: Int) = {
      val array = new Array[Long](range)
      var i = 0
      while (i < range) {
        array(i) = i.toLong
        i += 1
      }
      array
    }

    v = ArrayView(fillArray(size))
    vLo = ArrayView(fillArray(vLoSize))
  }

  @Benchmark
  def sum (bh: Blackhole) = {
    val ret : Long = v
      .foldLeft(0L)(_+_)
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
  def mapsMegamorphic (bh: Blackhole) = {
    val ret : Long = v
      .map(x => x + 1)
      .map(x => x + 1)
      .map(x => x + 1)
      .map(x => x + 1)
      .map(x => x + 1)
      .map(x => x + 1)
      .map(x => x + 1)
      .foldLeft(0L)(_+_)
    bh.consume(ret)
  }

  @Benchmark
  def filtersMegamorphic (bh: Blackhole) = {
    val ret : Long = v
      .filter(x => x > 1)
      .filter(x => x > 2)
      .filter(x => x > 3)
      .filter(x => x > 4)
      .filter(x => x > 5)
      .filter(x => x > 6)
      .filter(x => x > 7)
      .foldLeft(0L)(_+_)
    bh.consume(ret)
  }

  @Benchmark
  def maps (bh: Blackhole) = {
    val function = (x : Long) => { x + 1 }
    val ret : Long = v
      .map(function)
      .map(function)
      .map(function)
      .map(function)
      .map(function)
      .map(function)
      .map(function)
      .foldLeft(0L)(_+_)
    bh.consume(ret)
  }

  @Benchmark
  def cart (bh: Blackhole) = {
    val ret : Long = v
      .flatMap(d => vLo.view.map (dp => dp * d))
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
