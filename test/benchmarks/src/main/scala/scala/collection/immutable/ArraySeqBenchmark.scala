package scala.collection.immutable

import java.util.concurrent.TimeUnit
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
  var newS: Array[String] = _

  @Setup(Level.Trial) def initNumbers: Unit = {
    val integers = (1 to size).toList
    val strings = integers.map(_.toString)
    integersS = ArraySeq.unsafeWrapArray(integers.toArray)
    stringsS = ArraySeq.unsafeWrapArray(strings.toArray)
    newS = Array("a", "b", "c", "d", "e", "f")
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
    if (len == 1) b ++= seq
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

  // newS is used to avoid allocating Strings, while still performing some sort of "mapping".

  @Benchmark def mapSOld(): ArraySeq[AnyRef] =
    oldMap(stringsS)(x => newS(x.length))

  @Benchmark def mapSNew(): ArraySeq[AnyRef] =
    stringsS.map(x => newS(x.length))

  // Mapping an ArraySeq.ofInt results in an ArraySeq.ofRef containing java.lang.Integers.
  // Boxing small integers doesn't result in allocations thus the choice of _ & 0xf as the mapping function.

  @Benchmark def mapIOld(): ArraySeq[Int] =
    oldMap(integersS)(_ & 0xf)

  @Benchmark def mapINew(): ArraySeq[Int] =
    integersS.map(_ & 0xf)

  private def oldMap[A, B](seq: ArraySeq[A])(f: A => B): ArraySeq[B] =
    seq.iterableFactory.tabulate(seq.length)(i => f(seq.apply(i)))

  @Benchmark def `min-max is reduction`(bh: Blackhole): Unit = bh.consume {
    integersS.max
  }

}
