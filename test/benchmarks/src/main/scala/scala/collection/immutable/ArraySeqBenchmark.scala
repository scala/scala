package scala.collection.immutable

import java.util.concurrent.TimeUnit
import java.util.Arrays

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.reflect.ClassTag

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ArraySeqBenchmark {

  @Param(Array("0", "1", "10", "1000", "10000"))
  var size: Int = _
  var integersS: ArraySeq[Int] = _
  var stringsS: ArraySeq[String] = _

  @Setup(Level.Trial) def initNumbers: Unit = {
    val integers = (1 to size).toList
    val strings = integers.map(_.toString)
    integersS = ArraySeq.unsafeWrapArray(integers.toArray)
    stringsS = ArraySeq.unsafeWrapArray(strings.toArray)
  }

  @Benchmark def sortedStringOld(bh: Blackhole): Unit =
    bh.consume(oldSorted(stringsS))

  @Benchmark def sortedIntOld(bh: Blackhole): Unit =
    bh.consume(oldSorted(integersS))

  @Benchmark def sortedIntCustomOld(bh: Blackhole): Unit =
    bh.consume(oldSorted(integersS)(Ordering.Int.reverse, implicitly))

  @Benchmark def sortedStringNew(bh: Blackhole): Unit =
    bh.consume(stringsS.sorted)

  @Benchmark def sortedIntNew(bh: Blackhole): Unit =
    bh.consume(integersS.sorted)

  @Benchmark def sortedIntCustomNew(bh: Blackhole): Unit =
    bh.consume(integersS.sorted(Ordering.Int.reverse))

  private[this] def oldSorted[A](seq: ArraySeq[A])(implicit ord: Ordering[A], tag: ClassTag[A]): ArraySeq[A] = {
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
