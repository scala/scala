package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class HashMapEqualsBenchmark {
  @Param(Array("10", "100", "1000", "10000", "100000", "1000000"))
  var size: Int = _

  var base: HashMap[String, String] = _
  var notShared: HashMap[String, String] = _
  var identical: HashMap[String, String] = _
  var shared: HashMap[String, String] = _
  var differentShared: HashMap[String, String] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    base = (1 to size).map { i => s"key $i" -> s"value $i" }(scala.collection.breakOut)
    notShared = (1 to size).map { i => s"key $i" -> s"value $i" }(scala.collection.breakOut)
    identical = base
    shared = (base - base.head._1) + base.head
    differentShared = (base - base.last._1) + (base.last._1 -> (base.last._2 + "xx"))
  }


  @Benchmark
  def nonAllocatingIdentical() = {
    base == base
  }

  @Benchmark
  def nonAllocatingNotShared() = {
    base == notShared
  }

  @Benchmark
  def nonAllocatingShared() {
    base == shared
  }
  @Benchmark
  def nonAllocatingDifferentShared() {
    base == differentShared
  }
}
