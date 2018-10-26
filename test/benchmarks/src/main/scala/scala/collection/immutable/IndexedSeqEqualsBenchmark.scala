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
@Measurement(iterations = 10000)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class IndexedSeqEqualsBenchmark {
//  @Param(Array("0", "1", "2", "3", "4", "6", "8", "10", "12", "14", "16"
//    , "20", "24", "28", "32", "40", "48", "56", "64",
//    "128", "256", "512", "1024", "2048", "65536"))
  @Param(Array("128"))
  var size: Int = _

  @Param(Array("ArraySeq"))
  var type1: String = _
  @Param(Array("ArraySeq"))
  var type2: String = _

  var value: IndexedSeq[Any] = _
  var valueEqual: IndexedSeq[Any] = _
  var valueNotEqualFirst: IndexedSeq[Any] = _
  var valueNotEqualLast: IndexedSeq[Any] = _
  var valueNotEqualLength: IndexedSeq[Any] = _

  @Setup(Level.Trial) def init(): Unit = {
    def toSeq[T](data: Array[T], typeStr: String):IndexedSeq[T] = typeStr match {
      case "Vector" => data.to(Vector)
      case "ArraySeq" =>
        ArraySeq.unsafeWrapArray(data.clone)
    }

    val data = Array.tabulate(size)(_.toString)
    value = toSeq(data, type1)
    valueEqual = toSeq(data, type2)

    valueNotEqualLength = valueEqual + "xx"
    valueNotEqualFirst = if (value.isEmpty) valueEqual else valueEqual.updated(0, "xx")
    valueNotEqualLast = if (value.isEmpty) valueEqual else valueEqual.updated(valueEqual.length - 1, "xx")
  }
  @Benchmark def equal = value == valueEqual
//  @Benchmark def notEqualLength = value == valueNotEqualLength
//  @Benchmark def notEqualFirst = value == valueNotEqualFirst
//  @Benchmark def notEqualLast = value == valueNotEqualLast
}
