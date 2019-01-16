package scala.collection.mutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit
import java.util.{HashSet => JHashSet}

import scala.collection.convert.Wrappers
import scala.collection.convert.Wrappers.JSetWrapper

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class SetBenchmark2 {
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000", "1000000"))
  var size: Int = _
//  @Param(Array("HashSet"))//, "LinkedHashSet", "JSetWrapper"))
//  var setType: String = _

  var hashSet: HashSet[Int] = _
  var lhs: LinkedHashSet[Int] = _
  var jsw: JSetWrapper[Int] = _

  val pred: Int => Boolean = (i: Int) => i % 2 == 0

  @Setup(Level.Invocation) def init: Unit = {
    val elems = (0 until size)
    hashSet = elems.to(collection.mutable.HashSet)
    lhs = elems.to(collection.mutable.LinkedHashSet)
    val jSet = new JHashSet[Int]()
    elems.foreach(jSet.add)
    jsw =JSetWrapper(jSet)
  }

//  @Benchmark def filterInPlace_new(bh: Blackhole): Unit = {
//    bh.consume(set.filterInPlace(pred))
//  }
  @Benchmark def HashSet_super_filterInPlace(bh: Blackhole): Unit = {
    bh.consume(hashSet.superFilterInPlace(pred))
  }
  @Benchmark def HashSet_filterInPlace(bh: Blackhole): Unit = {
    bh.consume(hashSet.filterInPlace(pred))
  }
  @Benchmark def LinkedHashSet_super_filterInPlace(bh: Blackhole): Unit = {
    bh.consume(lhs.superFilterInPlace(pred))
  }
  @Benchmark def LinkedHashSet_filterInPlace(bh: Blackhole): Unit = {
    bh.consume(lhs.filterInPlace(pred))
  }
  @Benchmark def JSetWrapper_super_filterInPlace(bh: Blackhole): Unit = {
    bh.consume(jsw.superFilterInPlace(pred))
  }
  @Benchmark def JSetWrapper_filterInPlace(bh: Blackhole): Unit = {
    bh.consume(jsw.filterInPlace(pred))
  }
}
