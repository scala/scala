package scala.collection.immutable;

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.collection.mutable.ListBuffer

/** Benchmark some list operations to check the impact of making `::.tl` volatile. */
class VolatileListBenchmark {

  @Benchmark
  def buildSum10: Int = {
    val l = new ListBuffer[Int]
    var i = 0
    while(i < 10) {
      l += i
      i += 1
    }
    l.result.sum
  }

  @Benchmark
  def buildSum1000: Int = {
    val l = new ListBuffer[Int]
    var i = 0
    while(i < 1000) {
      l += i
      i += 1
    }
    l.result.sum
  }

  @Benchmark
  def buildImmutableSum1000: Int = {
    var l1: List[Int] = Nil
    var i = 0
    while(i < 1000) {
      l1 = i :: l1
      i += 1
    }
    l1.reverse.sum
  }

  @Benchmark
  def sum1000: Int = VolatileListBenchmark.l1000.sum
}

object VolatileListBenchmark {
  val l1000 = (0 to 1000).toList
}
