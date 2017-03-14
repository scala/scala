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
class SetBenchmark {

  var base: Set[String] = _


  @Setup(Level.Trial) def initKeys(): Unit = {
    base = Set("a", "b", "c", "d")
  }
  
  // immutable map is implemented as EmptySet -> Set1 -> Set2 -> Set3 -> Set4 -> HashSet
  // add an extra entry to Set4 causes a lot of work, benchmark the transition
  @Benchmark def set4AddElement(bh: Blackhole): Unit = {
    bh.consume(base + "e")
  }
}
