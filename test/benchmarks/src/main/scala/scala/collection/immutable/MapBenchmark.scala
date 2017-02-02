package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

object MapBenchmark {
  case class Content(value: Int)
}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class MapBenchmark {
  import MapBenchmark._
  @Param(Array("1", "10", "100", "1000"))
  var size: Int = _

  var values: List[Content] = _
  var map: Map[Content,Content] = _


  @Setup(Level.Trial) def initKeys(): Unit = {
    values = (1 until size).map { Content(_) }.toList
    map = values.filter(_.value % 2 == 0).map( v => (v,v)).toMap
  }

  @Benchmark def contains: Any = {
    values.foreach { v =>
      map.contains(v)
    }
  }
}
