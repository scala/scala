package scala.collection

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class DistinctBenchmark {
  @Param(Array("0", "1", "2", "5", "10", "20", "50", "100", "1000"))
  var size: Int = _

  @Param(Array("List", "Vector"))
  var collectionType: String = _

  var distinctDataSet: Seq[String] = null
  var lastDuplicatedDataSet: Seq[String] = null
  var firstDuplicatedDataSet: Seq[String] = null
  var interleavedDuplicationDataSet: Seq[String] = null
  var sequentialDuplicationDataSet: Seq[String] = null

  @Setup(Level.Trial) def init(): Unit = {
    val b1 = List.newBuilder[String]
    val b2 = List.newBuilder[String]
    0 until size foreach { i =>
      b1 += i.toString
      b2 += i.toString
      b2 += i.toString
    }

    val adjustCollectionType = collectionType match {
      case "List" => (col: Seq[String]) => col.toList
      case "Vector" => (col: Seq[String]) => col.toVector
    }

    distinctDataSet = adjustCollectionType(b1.result())
    interleavedDuplicationDataSet = adjustCollectionType(b2.result())
    sequentialDuplicationDataSet = adjustCollectionType(distinctDataSet ++ distinctDataSet)

    if (size > 0) {
      firstDuplicatedDataSet = adjustCollectionType(distinctDataSet.head +: distinctDataSet)
      lastDuplicatedDataSet = adjustCollectionType(distinctDataSet :+ distinctDataSet.head)
    }
  }

  @Benchmark def testDistinct: Any = {
    distinctDataSet.distinct
  }

  @Benchmark def testFirstDuplicated: Any = {
    firstDuplicatedDataSet.distinct
  }

  @Benchmark def testLastDuplicated: Any = {
    lastDuplicatedDataSet.distinct
  }

  @Benchmark def testInterleavedDuplication: Any = {
    interleavedDuplicationDataSet.distinct
  }

  @Benchmark def testSequentialDuplication: Any = {
    sequentialDuplicationDataSet.distinct
  }
}
