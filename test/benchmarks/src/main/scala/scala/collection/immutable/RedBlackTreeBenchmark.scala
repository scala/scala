package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class RedBlackTreeBenchmark {

  @Param(Array("0", "1", "10", "100", "1000", "10000"))
  var size: Int = _

  var nums: Range = _
  val rnd = new Random(0)
  var set1: TreeSet[Int] = _
  var perm: Array[Int] = _ // repeatably pseudo-random permutation

  @Setup(Level.Trial) def init: Unit = {
    nums = 1 to size
    set1 = TreeSet.from(nums)
    perm = new Array[Int](size)
    val rem = scala.collection.mutable.ArrayBuffer.from(nums)
    perm = Array.fill(size)(rem.remove(rnd.nextInt(rem.size)))
    assert(rem.size == 0)
    assert(perm.sum == nums.sum)
  }

  @Benchmark
  def build(bh: Blackhole): Unit =
    bh.consume(TreeSet.from(nums))

  @Benchmark
  def buildRandom(bh: Blackhole): Unit =
    bh.consume(TreeSet.from(perm))

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
    perm.foreach(i => s = s.excl(i))
    bh.consume(s)
  }
}
