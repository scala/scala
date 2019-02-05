package scala.collection.mutable

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
class AnyRefMapBenchmark {
  @Param(Array("10", "100", "1000"))
  var size: Int = _
  @Param(Array("true"))
  var useMissingValues = true

  var existingKeys: Array[String] = _
  var missingKeys: Array[String] = _
  var map: collection.mutable.AnyRefMap[String, String] = null

  @Setup(Level.Trial) def initialize: Unit = {
    existingKeys = (0 to size).map(_.toString).toArray
    missingKeys = (size to 2 * size).toArray.map(_.toString)
    map = collection.mutable.AnyRefMap(existingKeys.map(x => (x, x)) : _*)
  }

  @Benchmark def contains(bh: Blackhole): Unit = {
    var i = 0;
    while (i < size) {
      bh.consume(map.contains(existingKeys(i)))
      if (useMissingValues) {
        bh.consume(map.contains(missingKeys(i)))
      }
      i += 1
    }
  }

  @Benchmark def get(bh: Blackhole): Unit = {
    var i = 0;
    while (i < size) {
      bh.consume(map.get(existingKeys(i)))
      if (useMissingValues) {
        bh.consume(map.get(missingKeys(i)))
      }
      i += 1
    }
  }

  @Benchmark def getOrElse(bh: Blackhole): Unit = {
    var i = 0;
    while (i < size) {
      bh.consume(map.getOrElse(existingKeys(i), ""))
      if (useMissingValues) {
        bh.consume(map.getOrElse(missingKeys(i), ""))
      }
      i += 1
    }
  }

  @Benchmark def getOrElseUpdate(bh: Blackhole): Unit = {
    var i = 0;
    while (i < size) {
      bh.consume(map.getOrElseUpdate(existingKeys(i), ""))
      if (useMissingValues) {
        bh.consume(map.getOrElse(missingKeys(i), ""))
      }
      i += 1
    }
  }

  @Benchmark def fill(bh: Blackhole): Unit = {
    val h = new AnyRefMap[String, String]
    existingKeys.foreach(k => h.put(k, k))
    bh.consume(h)
  }
}
