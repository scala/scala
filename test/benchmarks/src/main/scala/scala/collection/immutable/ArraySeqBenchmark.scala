package scala.collection.immutable

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.reflect.ClassTag
import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ArraySeqBenchmark {
  import ListBenchmark._
  @Param(Array("0", "1", "10", "100", "1000"))
  var size: Int = _

  var intValues: ArraySeq[Int] = _
  var refValues: ArraySeq[Option[Int]] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    intValues = ArraySeq.fill(size)(Random.nextInt)
    refValues = intValues.map(Some(_))
  }

  @Benchmark def sortedNewInt: Any = {
    intValues.sorted
  }

  @Benchmark def sortedOldInt: Any = {
    sortedOldImpl(intValues)
  }

  @Benchmark def sortedNewRef: Any = {
    refValues.sorted
  }

  @Benchmark def sortedOldRef: Any = {
    sortedOldImpl(refValues)
  }

  // https://github.com/scala/scala/blob/eaeda754e2/src/library/scala/collection/Seq.scala#L590-L610
  private[this] def sortedOldImpl[A](seq: ArraySeq[A])(implicit ord: Ordering[A], tag: ClassTag[A]): ArraySeq[A] = {
    val len = seq.length
    val b = ArraySeq.newBuilder[A](tag)
    if (len == 1) b ++= seq.toIterable
    else if (len > 1) {
      b.sizeHint(len)
      val arr = new Array[AnyRef](len)
      var i = 0
      for (x <- seq) {
        arr(i) = x.asInstanceOf[AnyRef]
        i += 1
      }
      java.util.Arrays.sort(arr, ord.asInstanceOf[Ordering[Object]])
      i = 0
      while (i < arr.length) {
        b += arr(i).asInstanceOf[A]
        i += 1
      }
    }
    b.result()
  }
}
