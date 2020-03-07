package scala.collection.immutable

import java.util
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class RedBlackTreeBenchmark {

  @Param(Array("0", "1", "10", "100", "1000", "10000"))
  var size: Int = _

  var nums: Range = _
  val rnd = new Random(0)
  var set1: TreeSet[Int] = _
  var set2: TreeSet[Int] = _
  var set3: TreeSet[Int] = _
  var set4: TreeSet[Int] = _
  var perm: Array[Int] = _ // repeatably pseudo-random permutation
  var map1: TreeMap[Int, Int] = _

  @Setup(Level.Trial) def init: Unit = {
    nums = 1 to size
    set1 = nums.to[TreeSet]
    perm = nums.toArray
    util.Collections.shuffle(util.Arrays.asList(perm))
    set2 = set1.take(size/4)
    set3 = set1.take(size*3/4)
    set4 = set1.drop(size/2)
    map1 = set1.map(i => (i -> i))(collection.breakOut)
  }

  @Benchmark
  def build(bh: Blackhole): TreeSet[Int] =
    (TreeSet.newBuilder[Int] ++= nums).result

  @Benchmark
  def buildRandom(bh: Blackhole): Unit =
    (TreeSet.newBuilder[Int] ++= perm).result

  @Benchmark
  def iterator(bh: Blackhole): Unit = {
    val it = set1.iterator
    var res = 0
    while(it.hasNext)
      res += it.next()
    bh.consume(res)
  }

  @Benchmark
  def foreach(bh: Blackhole): Unit = {
    var i = 0
    set1.foreach { x => i += x }
    bh.consume(i)
  }

  @Benchmark
  def drain(bh: Blackhole): Unit = {
    var s = set1
    perm.foreach(i => s = s.-(i))
    bh.consume(s)
  }

  @Benchmark
  def union(bh: Blackhole): Unit = {
    bh.consume(
      set1.union(set1).size +
        set2.union(set3).size +
        set2.union(set4).size +
        set4.union(set2).size
    )
  }

  @Benchmark
  def range(bh: Blackhole): Unit = {
    val s = set1
    var res = 0
    for(i <- 0 to 5; j <- 0 to 5) res += s.range(s.size*i/5, s.size*j/5).size
    bh.consume(res)
  }

  @Benchmark
  def slice(bh: Blackhole): Unit = {
    val s = set1
    var res = 0
    for(i <- 0 to 5; j <- 0 to 5) res += s.slice(s.size*i/5, s.size*j/5).size
    bh.consume(res)
  }

  @Benchmark
  def take(bh: Blackhole): Unit = {
    val s = set1
    var res = 0
    for(i <- 0 to 10) res += s.take(s.size*i/10).size
    bh.consume(res)
  }

  @Benchmark
  def drop(bh: Blackhole): Unit = {
    val s = set1
    var res = 0
    for(i <- 0 to 10) res += s.drop(s.size*i/10).size
    bh.consume(res)
  }

  @Benchmark
  def filterKeep(bh: Blackhole): Unit =
    bh.consume(set1.filter(_ => true))

  @Benchmark
  def filterDrop(bh: Blackhole): Unit =
    bh.consume(set1.filter(_ => false))

  @Benchmark
  def filterPartial(bh: Blackhole): Unit =
    bh.consume(set1.filter(i => i % 2 == 0))

  @Benchmark
  def partition(bh: Blackhole): Unit =
    bh.consume(set1.partition(i => i % 2 == 0))

  @Benchmark
  def transformNone(bh: Blackhole): Unit =
    bh.consume(map1.transform((k, v) => v))

  @Benchmark
  def transformAll(bh: Blackhole): Unit =
    bh.consume(map1.transform((k, v) => v+1))

  @Benchmark
  def transformHalf(bh: Blackhole): Unit =
    bh.consume(map1.transform((k, v) => if(k % 2 == 0) v else v+1))

  @Benchmark
  def tails(bh: Blackhole): Unit = {
    val it = set1.tails
    while(it.hasNext) bh.consume(it.next())
  }
}
