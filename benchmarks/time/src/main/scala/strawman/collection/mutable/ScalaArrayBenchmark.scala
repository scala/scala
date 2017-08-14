package strawman.collection.mutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.{Any, AnyRef, Int, Long, Unit, math}
import scala.Predef.{intWrapper, longArrayOps, wrapLongArray, wrapRefArray}

import scala.Predef.{intWrapper, longArrayOps}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class ScalaArrayBenchmark {

  @Param(scala.Array(/*"0", */"1"/*, "2", "3", "4", "7"*/, "8"/*, "15", "16"*/, "17"/*, "39"*/, "282", "4096", "31980"/*, "73121", "120000"*/))
  var size: Int = _

  var xs: scala.Array[Long] = _
  var xss: scala.Array[scala.Array[Long]] = _
  var zipped: scala.Array[(Long, Long)] = _
  var randomIndices: scala.Array[Int] = _
  var randomIndices2: scala.Array[Int] = _
  var randomXss: scala.Array[scala.Array[Long]] = _

  @Setup(Level.Trial)
  def initData(): Unit = {
    def freshCollection() = scala.Array((1 to size).map(_.toLong): _*)
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
  def prepend(bh: Blackhole): Unit = {
    var ys = scala.Array.empty[Long]
    var i = 0L
    while (i < size) {
      ys = i +: ys
      i = i + 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def append(bh: Blackhole): Unit = {
    var ys = scala.Array.empty[Long]
    var i = 0L
    while (i < size) {
      ys = ys :+ i
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def prependAppend(bh: Blackhole): Unit = {
    var ys = scala.Array.empty[Long]
    var i = 0L
    while (i < size) {
      if ((i & 1) == 1) ys = ys :+ i
      else ys = i +: ys
      i = i + 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def prependAll(bh: Blackhole): Unit = bh.consume(xs ++: xs)

  @Benchmark
  def appendAll(bh: Blackhole): Unit = bh.consume(xs ++ xs)

  @Benchmark
  def prependAllAppendAll(bh: Blackhole): Unit = {
    var ys = scala.Array.empty[Long]
    val ys2 = xss(0).take(3)
    var i = 0L
    while (i < size) {
      if ((i & 1) == 1) ys = ys ++ ys2
      else ys = ys2 ++: ys
      i = i + 1
    }
    bh.consume(ys)
  }

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
  def loop_iterator(bh: Blackhole): Any = {
    var n = 0
    val it = xs.iterator
    while (it.hasNext) {
      bh.consume(it.next())
      n += 1
    }
    bh.consume(n)
  }

  @Benchmark
  def distinct(bh: Blackhole): Unit = bh.consume(xs.distinct)

  @Benchmark
  @OperationsPerInvocation(1000)
  def lookup_last(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xss(i)(size - 1))
      i = i + 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def lookup_random(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs(randomIndices(i)))
      i = i + 1
    }
  }

  @Benchmark
  def map(bh: Blackhole): Unit = bh.consume(xs.map(x => x + 1))

  @Benchmark
  @OperationsPerInvocation(1000)
  def patch(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      val from = randomIndices(i)
      val replaced = randomIndices2(i)
      bh.consume(xs.patch(from, randomXss.apply(i), replaced))
      i = i + 1
    }
  }

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
  def unzip(bh: Blackhole): Unit = bh.consume(zipped.unzip(t => (t._1, t._2)))

  @Benchmark
  def padTo(bh: Blackhole): Unit = bh.consume(xs.padTo(size * 2, 42))

  @Benchmark
  def reverse(bh: Blackhole): Any = bh.consume(xs.reverse)

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

  @Benchmark
  @OperationsPerInvocation(1000)
  def updated(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs.updated(randomIndices(i), i))
      i = i + 1
    }
  }
}