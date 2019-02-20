package scala.collection.mutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit
import java.util.{ HashSet => JHashSet }

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 20)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BuilderBenchmark {

  import collection.mutable

  var listBuffer: mutable.ListBuffer[String] = _
  var arrayBuffer: mutable.ArrayBuffer[String] = _
  var queue: mutable.Queue[String] = _
  var sinks: Array[mutable.Buffer[String]] = _
  var source1: collection.Seq[String] = _
  var source2: collection.Seq[String] = _
  var source3: collection.Seq[String] = _
  var sources: Array[collection.Seq[String]] = _

  @Setup(Level.Iteration) def init: Unit = {
    listBuffer = new mutable.ListBuffer
    arrayBuffer = new mutable.ArrayBuffer
    queue = new collection.mutable.Queue
    source1 = (1 to 1000).map(_.toString).toList
    source2 = (1 to 1000).map(_.toString).toArray[String]
    source3 = (1 to 1000).map(_.toString).toVector
    sources = scala.util.Random.shuffle(List.fill(10)(List(source1, source2, source2, source3)).flatten).toArray
    sinks = Array[Buffer[String]](listBuffer, arrayBuffer, listBuffer, queue, listBuffer, arrayBuffer, listBuffer, listBuffer)
  }

  @Benchmark def addAllPolymorphic(bh: Blackhole): Unit = {
    var i, j = 0
    val sources = this.sources
    val sinks = this.sinks
    while (i < sinks.length) {
      sinks(i).clear()
      while (j < sources.length) {
        sinks(i).addAll(sources(j))
        j += 1
      }
      bh.consume(sinks(i))
      i += 1
    }
  }
}
