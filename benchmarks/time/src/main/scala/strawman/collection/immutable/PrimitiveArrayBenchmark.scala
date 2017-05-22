package strawman.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import scala.{Any, AnyRef, Int, Unit}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class PrimitiveArrayBenchmark {

  @Param(scala.Array("8", "64", "512", "4096", "32768", "262144"/*, "2097152"*/))
  var size: Int = _

  var xs: ImmutableArray[Int] = _
  var obj: Int = _

  @Setup(Level.Trial)
  def initData(): Unit = {
    xs = ImmutableArray.fill(size)(obj)
    obj = 123
  }

  @Benchmark
  def cons(): Any = {
    var ys = ImmutableArray.empty[Int]
    var i = 0
    while (i < size) {
      ys = ys :+ obj
      i += 1
    }
    ys
  }

  @Benchmark
  def uncons(): Any = xs.tail

  @Benchmark
  def concat(): Any = xs ++ xs

  @Benchmark
  def foreach(): Any = {
    var n = 0
    xs.foreach(x => if (x == 0) n += 1)
    n
  }

  @Benchmark
  def lookup(): Any = xs(size - 1)

  @Benchmark
  def map(): Any = xs.map(x => if (x == 0) "foo" else "bar")

}
