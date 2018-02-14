package benchmarks.immutableHashSet

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import strawman.collection.immutable

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 6)
@Measurement(iterations = 6)
@State(Scope.Benchmark)
class HashSet {
  @Param(scala.Array("0", "1", "3", "8", "17", "282", "4096", "131070", "7312102"))
  var size: Int = _

  var xs: immutable.HashSet[Long] = _
  var ys: immutable.HashSet[Long] = _
  def fresh(n: Int) = immutable.HashSet((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(size)
    ys = xs.take(xs.size / 2)
  }

  @Benchmark
  def subsetOf(bh: Blackhole) = bh.consume(ys.subsetOf(xs))

  @Benchmark
  def equals(bh: Blackhole) = bh.consume(xs == ys)

  @Benchmark
  def incl(bh: Blackhole) = bh.consume(xs + 42)

  @Benchmark
  def excl(bh: Blackhole) = bh.consume(xs - 42)

  @Benchmark
  def union(bh: Blackhole) = bh.consume(xs union ys)

  @Benchmark
  def intersection(bh: Blackhole) = bh.consume(xs intersect ys)

}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 6)
@Measurement(iterations = 6)
@State(Scope.Benchmark)
class ChampHashSet {
  @Param(scala.Array("0", "1", "3", "8", "17", "282", "4096", "131070", "7312102"))
  var size: Int = _

  var xs: immutable.ChampHashSet[Long] = _
  var ys: immutable.ChampHashSet[Long] = _
  def fresh(n: Int) = immutable.ChampHashSet((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(size)
    ys = xs.take(xs.size / 2)
  }

  @Benchmark
  def subsetOf(bh: Blackhole) = bh.consume(ys.subsetOf(xs))

  @Benchmark
  def equals(bh: Blackhole) = bh.consume(xs == ys)

  @Benchmark
  def incl(bh: Blackhole) = bh.consume(xs + 42)

  @Benchmark
  def excl(bh: Blackhole) = bh.consume(xs - 42)

  @Benchmark
  def union(bh: Blackhole) = bh.consume(xs union ys)

  @Benchmark
  def intersection(bh: Blackhole) = bh.consume(xs intersect ys)

}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 6)
@Measurement(iterations = 6)
@State(Scope.Benchmark)
class OldHashSet {
  @Param(scala.Array("0", "1", "3", "8", "17", "282", "4096", "131070", "7312102"))
  var size: Int = _

  var xs: scala.collection.immutable.HashSet[Long] = _
  var ys: scala.collection.immutable.HashSet[Long] = _
  def fresh(n: Int) = scala.collection.immutable.HashSet((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(size)
    ys = xs.take(xs.size / 2)
  }

  @Benchmark
  def subsetOf(bh: Blackhole) = bh.consume(ys.subsetOf(xs))

  @Benchmark
  def equals(bh: Blackhole) = bh.consume(xs == ys)

  @Benchmark
  def incl(bh: Blackhole) = bh.consume(xs + 42)

  @Benchmark
  def excl(bh: Blackhole) = bh.consume(xs - 42)

  @Benchmark
  def union(bh: Blackhole) = bh.consume(xs union ys)

  @Benchmark
  def intersection(bh: Blackhole) = bh.consume(xs intersect ys)

}
