package scala.collection.mutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit

import scala.collection.mutable

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

  var map = new mutable.HashMap[Any, Any]

  @Setup(Level.Invocation) def initializeMutable = existingKeys.foreach(v => map.put(v, v))

  @TearDown(Level.Invocation) def tearDown = map.clear()

  @Benchmark def getOrElseUpdate(bh: Blackhole): Unit = {
    var i = 0;
    while (i < size) {
      bh.consume(map.getOrElseUpdate(existingKeys(i), -1))
      bh.consume(map.getOrElseUpdate(missingKeys(i), -1))
      i += 1
    }
  }

  @Benchmark def get(bh: Blackhole): Unit = {
    var i = 0;
    while (i < size) {
      bh.consume(map.get(existingKeys(i), -1))
      bh.consume(map.get(missingKeys(i), -1))
      i += 1
    }
  }

  @Benchmark def put(bh: Blackhole): Any = {
    var map = new mutable.HashMap[Any, Any]

    var i = 0;
    while (i < size) {
      map.put(existingKeys(i), i)
      i += 1
    }

    map
  }
}
