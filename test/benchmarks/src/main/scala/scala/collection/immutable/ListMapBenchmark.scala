package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

//typical usage bench/jmh:run scala.collection.immutable.TreeMapBenchmark --prof gc --rf csv

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ListMapBenchmark {

  @Param(Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "100"))
  var size: Int = _

  var map1: ListMap[Int, Int] = _

  @Setup(Level.Trial) def init: Unit = {
    map1 = ListMap(List.tabulate(size)(i => (i -> i)): _*)
  }

  @Benchmark
  def iterator(bh: Blackhole): Unit = {
    val it = map1.iterator
    bh.consume(it)
  }

  @Benchmark
  def iteration(bh: Blackhole): Unit = {
    val it = map1.iterator
    bh.consume(it)
    while (it.hasNext) {
      bh.consume(it.next)
    }
  }
}

//for testing, debugging, optimising etc
object ListMapTest extends App {

  val bh = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.")
  val test = new ListMapBenchmark
  test.size = 8
  test.init

  while (true) {
    var j = 0
    val start = System.nanoTime()
    while (j < 10000000) {
      test.iteration(bh)
      j += 1
    }
    val end = System.nanoTime()
    println((end - start) / 1000000)
  }
}
