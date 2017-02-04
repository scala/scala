package scala.collection.immutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class HashMapBenchmark {
  @Param(Array("10", "100", "1000"))
  var size: Int = _

  var existingKeys: Array[Any] = _
  var missingKeys: Array[Any] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    existingKeys = (0 to size).map(i => (i % 4) match {
      case 0 => i.toString
      case 1 => i.toChar
      case 2 => i.toDouble
      case 3 => i.toInt
    }).toArray
    missingKeys = (size to 2 * size).toArray
  }

  var map: collection.immutable.HashMap[Any, Any] = null

  @Setup(Level.Trial) def initialize = {
    map = collection.immutable.HashMap(existingKeys.map(x => (x, x)) : _*)
  }

  @Benchmark def contains(bh: Blackhole): Unit = {
    var i = 0;
    while (i < size) {
      bh.consume(map.contains(existingKeys(i)))
      bh.consume(map.contains(missingKeys(i)))
      i += 1
    }
  }

  @Benchmark def get(bh: Blackhole): Unit = {
    var i = 0;
    while (i < size) {
      bh.consume(map.get(existingKeys(i)))
      bh.consume(map.get(missingKeys(i)))
      i += 1
    }
  }
}
