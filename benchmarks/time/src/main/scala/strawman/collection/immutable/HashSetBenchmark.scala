package strawman.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.{Any, AnyRef, Int, Unit}
import scala.Predef.intWrapper

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class HashSetBenchmark {

  @Param(scala.Array("8", "64", "512", "4096", "32768", "262144"/*, "2097152"*/))
  var size: Int = _

  var xs: HashSet[AnyRef] = _
  var obj: Any = _

  @Setup(Level.Trial)
  def initData(): Unit = {
    xs = HashSet((1 to size).map(_.toString): _*)
    obj = ""
  }

  @Benchmark
  def cons(): Any = {
    var ys = HashSet.empty[Any]
    var i = 0
    while (i < size) {
      ys = ys + obj // Note: we should test different cases: colliding values as well as non-colliding values, etc.
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
    xs.foreach(x => if (x eq null) n += 1)
    n
  }

  @Benchmark
  def map(): Any = xs.map(x => if (x eq null) "foo" else "bar")

}
