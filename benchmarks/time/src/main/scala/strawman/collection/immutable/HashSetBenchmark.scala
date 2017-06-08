package strawman.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.{Any, AnyRef, Int, Long, Unit}
import scala.Predef.intWrapper

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class HashSetBenchmark {

  @Param(scala.Array(/*"0", */"1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "73121", "7312102"))
  var size: Int = _

  var xs: HashSet[Long] = _
  var xss: scala.Array[HashSet[Long]] = _
  var randomIndices: scala.Array[Int] = _

  @Setup(Level.Trial)
  def initData(): Unit = {
    def freshCollection() = HashSet((1 to size).map(_.toLong): _*)
    xs = freshCollection()
//    xss = scala.Array.fill(1000)(freshCollection())
//    if (size > 0) {
//      randomIndices = scala.Array.fill(1000)(scala.util.Random.nextInt(size))
//    }
  }

  @Benchmark
//  @OperationsPerInvocation(size)
  def cons(bh: Blackhole): Unit = {
    var ys = HashSet.empty[Long]
    var i = 0L
    while (i < size) {
      ys = ys + i
      i = i + 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def uncons(bh: Blackhole): Unit = bh.consume(xs.tail)

  @Benchmark
  def concat(bh: Blackhole): Unit = bh.consume(xs ++ xs)

  @Benchmark
  def foreach(bh: Blackhole): Unit = {
    xs.foreach(x => bh.consume(x))
  }

  @Benchmark
  def map(bh: Blackhole): Unit = bh.consume(xs.map(x => x + 1))

  @Benchmark
  def groupBy(bh: Blackhole): Unit = {
    val result = xs.groupBy(_ % 5)
    bh.consume(result)
  }

}
