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
class HashSetEqualsBenchmark {
  @Param(Array("10", "100", "1000", "10000", "100000", "1000000"))
  var size: Int = _

  var base: HashSet[String] = _
  var notShared: HashSet[String] = _
  var identical: HashSet[String] = _
  var shared: HashSet[String] = _
  var differentShared: HashSet[String] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    base = (1 to size).map { i => s"key $i" }(scala.collection.breakOut)
    notShared = (1 to size).map { i => s"key $i"  }(scala.collection.breakOut)
    identical = base
    shared = (base - base.head) + base.head
    differentShared = (base - base.last) + (base.last + "xx")
  }


  @Benchmark
  def bmIdentical() = {
    base == base
    base .equals(base)
  }

  @Benchmark
  def bmNotShared() = {
    base == notShared
  }

  @Benchmark
  def bmShared() {
    base == shared
  }
  @Benchmark
  def bmDifferentShared() {
    base == differentShared
  }
}
