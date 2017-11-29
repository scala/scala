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
class VectorBenchmark {
  @Param(Array("1", "10", "100", "1000", "10000"))
  var size: Int = _

  var values: Vector[Any] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    values = (0 to size).map(i => (i % 4) match {
      case 0 => i.toString
      case 1 => i.toChar
      case 2 => i.toDouble
      case 3 => i.toInt
    }).toVector
  }

  @Benchmark
  def builderConcat() = {
    val builder = Vector.newBuilder[Any]
    val loopSize = 1000
    var i = 0
    while (i < loopSize) {
      builder ++= values
      i += 1
    }
    builder.result()
  }
}
