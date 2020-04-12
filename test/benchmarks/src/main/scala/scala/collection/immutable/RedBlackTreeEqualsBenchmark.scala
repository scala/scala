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
class RedBlackTreeEqualsSharedBenchmark {

  @Param(Array("0", "1", "10", "100", "1000", "10000"))
  var size: Int = _

  @Param(Array("0", "1", "10"))
  var diff: Int = _

  var set: TreeSet[Int] = _
  var otherSet: TreeSet[Int] = _
  var map: TreeMap[Int, Int] = _
  var otherMap: TreeMap[Int, Int] = _

  @Setup(Level.Trial) def init: Unit = {
    val r = new Random()
    r.setSeed(0x1234567890abcdefL)

    def aSet(start: Int, end: Int) : TreeSet[Int] = (start to end).to[TreeSet]
    def aMap(start: Int, end: Int) : TreeMap[Int, Int] = TreeMap.empty[Int, Int] ++ ((start to end) map {x => x-> x})

    set = aSet(1, size)
    otherSet = (1 to diff).foldLeft(set) {
      case (s,n) => val n = r.nextInt(size)
      s - n + n
    }

    map = aMap(1, size)
    otherMap =(1 to diff).foldLeft(map) {
      case (s,n) => val n = r.nextInt(size)
        s - n + (n->n)
    }
  }

  @Benchmark
  def setEqualsOther =
    set == otherSet

  @Benchmark
  def mapEqualsOther =
    map == otherMap
}
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class RedBlackTreeEqualsUnsharedBenchmark {

  @Param(Array("10", "100", "1000", "10000"))
  var size: Int = _

  var set: TreeSet[Int] = _
  var otherSet: TreeSet[Int] = _
  var map: TreeMap[Int, Int] = _
  var otherMap: TreeMap[Int, Int] = _

  @Setup(Level.Trial) def init: Unit = {
    def aSet(start: Int, end: Int) : TreeSet[Int] = (start to end).to[TreeSet]
    def aMap(start: Int, end: Int) : TreeMap[Int, Int] = TreeMap.empty[Int, Int] ++ ((start to end) map {x => x-> x})
    set = aSet(1, size)
    otherSet =  aSet(1, size)

    map = aMap(1, size)
    otherMap = aMap(1, size)
  }

  @Benchmark
  def setEqualsOther =
    set == otherSet

  @Benchmark
  def mapEqualsOther =
    map == otherMap
}
object Mike extends App {
  val x = new RedBlackTreeEqualsUnsharedBenchmark
  x.size = 10000
  x.init
  var i = 0
  var start = System.currentTimeMillis()
  while (true ) {
    x.mapEqualsOther
    i += 1
    if (i == 1000) {
      val now = System.currentTimeMillis()
      println(s"${now - start}")
      i = 0
      start = now
    }
  }
}
