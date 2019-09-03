package scala.collection.mutable

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
class LinkedHashMapBenchmark {
  @Param(Array("10", "100", "1000"))
  var size: Int = _
  @Param(Array("true"))
  var useMissingValues = true
  @Param(Array("false"))
  var stringsOnly = false

  var existingKeys: Array[Any] = _
  var missingKeys: Array[Any] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    existingKeys = (0 to size).map(i => (i % 4) match {
      case _ if stringsOnly => i.toString
      case 0 => i.toString
      case 1 => i.toChar
      case 2 => i.toDouble
      case 3 => i.toInt
    }).toArray
    missingKeys = (size to 2 * size).toArray.map(_.toString)
  }

  var map: collection.mutable.LinkedHashMap[Any, Any] = null

  @Setup(Level.Trial) def initialize = {
    map = collection.mutable.LinkedHashMap(existingKeys.map(x => (x, x)) : _*)
  }

  @Benchmark def updateWith(bh: Blackhole): Unit = {
    var i = 0;
    while (i < size) {
      val res = i % 4 match {
        case 0 => map.updateWith(existingKeys(i % existingKeys.length))(_ => None)
        case 1 => map.updateWith(existingKeys(i % existingKeys.length))(_ => Some(existingKeys(i % existingKeys.length)))

        case 2 => map.updateWith(missingKeys(i % missingKeys.length))(_ => None)
        case 3 => map.updateWith(missingKeys(i % missingKeys.length))(_ => Some(existingKeys(i % existingKeys.length)))
      }
      bh.consume(res)
      i += 1
    }
  }
}
