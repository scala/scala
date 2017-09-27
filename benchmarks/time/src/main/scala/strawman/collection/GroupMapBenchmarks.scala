package strawman.collection

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole


@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class GroupMapBenchmark {

  type CC[A] = immutable.List[A]
  val factory = immutable.List

  @Param(scala.Array("2", "3", "5", "16", "17", "32", "33", "128", "129"))
  var size: Int = _

  var xs: CC[Long] = _

  def fresh(n: Int) = factory((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(10000)
  }

  @Benchmark
  def groupMap(bh: Blackhole): Unit =
    xs.groupMap(_ % size)(_ * 2).foreach(bh.consume)

}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class GroupMapValuesBenchmark {

  type CC[A] = immutable.List[A]
  val factory = immutable.List

  @Param(scala.Array("2", "3", "5", "16", "17", "32", "33", "128", "129"))
  var size: Int = _

  var xs: CC[Long] = _

  def fresh(n: Int) = factory((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(10000)
  }

  @Benchmark
  def groupMap(bh: Blackhole): Unit =
    xs.groupBy(_ % size).mapValues(_.map(_ * 2)).foreach(bh.consume)

}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class ScalaGroupMapValuesBenchmark {

  type CC[A] = scala.collection.immutable.List[A]
  val factory = scala.collection.immutable.List

  @Param(scala.Array("2", "3", "5", "16", "17", "32", "33", "128", "129"))
  var size: Int = _

  var xs: CC[Long] = _

  def fresh(n: Int) = factory((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(10000)
  }

  @Benchmark
  def groupMap(bh: Blackhole): Unit =
    xs.groupBy(_ % size).mapValues(_.map(_ * 2)).foreach(bh.consume)

}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class GroupMapReduceBenchmark {

  type CC[A] = immutable.List[A]
  val factory = immutable.List

  @Param(scala.Array("2", "3", "5", "16", "17", "32", "33", "128", "129"))
  var size: Int = _

  var xs: CC[Long] = _

  def fresh(n: Int) = factory((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(10000)
  }

  @Benchmark
  def groupMapReduce(bh: Blackhole): Unit =
    xs.groupMapReduce(_ % size)(_ * 2)(_ + _).foreach(bh.consume)

}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class GroupMapValuesReduceBenchmark {

  type CC[A] = immutable.List[A]
  val factory = immutable.List

  @Param(scala.Array("2", "3", "5", "16", "17", "32", "33", "128", "129"))
  var size: Int = _

  var xs: CC[Long] = _

  def fresh(n: Int) = factory((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(10000)
  }

  @Benchmark
  def groupMapReduce(bh: Blackhole): Unit =
    xs.groupBy(_ % size).mapValues(_.map(_ * 2).reduce(_ + _)).foreach(bh.consume)

}


@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class ScalaGroupMapValuesReduceBenchmark {

  type CC[A] = scala.collection.immutable.List[A]
  val factory = scala.collection.immutable.List

  @Param(scala.Array("2", "3", "5", "16", "17", "32", "33", "128", "129"))
  var size: Int = _

  var xs: CC[Long] = _

  def fresh(n: Int) = factory((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(10000)
  }

  @Benchmark
  def groupMapReduce(bh: Blackhole): Unit =
    xs.groupBy(_ % size).mapValues(_.map(_ * 2).reduce(_ + _)).foreach(bh.consume)

}
