package strawman.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.{Any, AnyRef, Int, Long, Unit, math}
import scala.Predef.intWrapper

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class TreeSetBenchmark {
  @Param(scala.Array("0", "1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "4096", "131070", "7312102"))
  var size: Int = _

  var xs: TreeSet[Long] = _
  var zs: TreeSet[Long] = _
  var zipped: TreeSet[(Long, Long)] = _
  var randomIndices: scala.Array[Int] = _
  def fresh(n: Int) = TreeSet((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(size)
    zs = fresh((size / 1000) max 2).map(-_)
    zipped = xs.map(x => (x, x))
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(scala.util.Random.nextInt(size))
    }
  }

  @Benchmark
  def create(bh: Blackhole): Unit = bh.consume(fresh(size))

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_incl(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys += -i
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def expand_concat(bh: Blackhole): Unit = bh.consume(xs ++ zs)

  @Benchmark
  def traverse_foreach(bh: Blackhole): Unit = xs.foreach(x => bh.consume(x))

  @Benchmark
  def traverse_headTail(bh: Blackhole): Unit = {
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.head)
      ys = ys.tail
    }
  }

  @Benchmark
  def traverse_initLast(bh: Blackhole): Unit = {
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.last)
      ys = ys.init
    }
  }

  @Benchmark
  def traverse_iterator(bh: Blackhole): Unit = {
    val it = xs.iterator()
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  def traverse_foldLeft(bh: Blackhole): Unit = bh.consume(xs.foldLeft(0) {
    case (acc, n) =>
      bh.consume(n)
      acc + 1
  })

  @Benchmark
  def traverse_foldRight(bh: Blackhole): Unit = bh.consume(xs.foldRight(0) {
    case (n, acc) =>
      bh.consume(n)
      acc - 1
  })
  @Benchmark
  def access_tail(bh: Blackhole): Unit = bh.consume(xs.tail)

  @Benchmark
  def access_init(bh: Blackhole): Unit = bh.consume(xs.init)

  @Benchmark
  @OperationsPerInvocation(100)
  def access_slice(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      bh.consume(xs.slice(size - size / (i + 1), size))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_contains(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs.contains(i))
      i += 1
    }
  }

  @Benchmark
  def transform_map(bh: Blackhole): Unit = bh.consume(xs.map(x => x + 1))

  @Benchmark
  @OperationsPerInvocation(100)
  def transform_span(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      val (xs1, xs2) = xs.span(x => x < randomIndices(i))
      bh.consume(xs1)
      bh.consume(xs2)
      i += 1
    }
  }

  @Benchmark
  def transform_unzip(bh: Blackhole): Unit = bh.consume(zipped.unzip)

  @Benchmark
  def transform_groupBy(bh: Blackhole): Unit = {
    val result = xs.groupBy(_ % 5)
    bh.consume(result)
  }
}