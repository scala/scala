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
class MapBenchmark {

  var base: Map[String,String] = _


  @Setup(Level.Trial) def initKeys(): Unit = {
    base = Map("a" -> "a", "b" -> "b", "c" -> "c", "d" -> "d")
  }

  // immutable map is implemented as EmptyMap -> Map1 -> Map2 -> Map3 -> Map4 -> Hashmap
  // add an extra entry to Map4 causes a lot of work, benchmark the transition
  @Benchmark def map4AddElement(bh: Blackhole): Unit = {
    bh.consume(base.updated("e", "e"))
  }
}
