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
  var zipped: HashSet[(Long, Long)] = _
  var randomIndices: scala.Array[Int] = _
  var randomIndices2: scala.Array[Int] = _
  var randomXss: scala.Array[HashSet[Long]] = _

  @Setup(Level.Trial)
  def initData(): Unit = {
    def freshCollection() = HashSet((1 to size).map(_.toLong): _*)
    xs = freshCollection()
    xss = scala.Array.fill(1000)(freshCollection())
    zipped = xs.map(x => (x, x))
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(scala.util.Random.nextInt(size))
      randomIndices2 = scala.Array.fill(1000)(scala.util.Random.nextInt(size))
      randomXss = scala.Array.fill(1000)(freshCollection().take(scala.util.Random.nextInt(size)))
    }
  }

  @Benchmark
  def incl(bh: Blackhole): Unit = {
    var ys = HashSet.empty[Long]
    var i = 0L
    while (i < size) {
      ys += i
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def concat(bh: Blackhole): Unit = bh.consume(xs ++ xs)

  @Benchmark
  def tail(bh: Blackhole): Unit = bh.consume(xs.tail)

  @Benchmark
  def init(bh: Blackhole): Unit = bh.consume(xs.init)

  @Benchmark
  def loop_foreach(bh: Blackhole): Unit = xs.foreach(x => bh.consume(x))

  @Benchmark
  def loop_headTail(bh: Blackhole): Unit = {
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.head)
      ys = ys.tail
    }
  }

  @Benchmark
  def loop_initLast(bh: Blackhole): Unit = {
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.last)
      ys = ys.init
    }
  }

  @Benchmark
  def loop_iterator(bh: Blackhole): Unit = {
    val it = xs.iterator()
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def contains(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xss(i).contains(i))
      i += 1
    }
  }

  @Benchmark
  def map(bh: Blackhole): Unit = bh.consume(xs.map(x => x + 1))

  @Benchmark
  @OperationsPerInvocation(100)
  def span(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      val (xs1, xs2) = xs.span(x => x < randomIndices(i))
      bh.consume(xs1)
      bh.consume(xs2)
      i += 1
    }
  }

  @Benchmark
  def unzip(bh: Blackhole): Unit = bh.consume(zipped.unzip)

  @Benchmark
  def foldLeft(bh: Blackhole): Any = bh.consume(xs.foldLeft(0) {
    case (acc, n) =>
      bh.consume(n)
      acc + 1
  })

  @Benchmark
  def foldRight(bh: Blackhole): Any = bh.consume(xs.foldRight(0) {
    case (n, acc) =>
      bh.consume(n)
      acc - 1
  })

  @Benchmark
  def groupBy(bh: Blackhole): Unit = {
    val result = xs.groupBy(_ % 5)
    bh.consume(result)
  }
}
