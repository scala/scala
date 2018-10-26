package scala.collection.mutable

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
class ArrayOpsBenchmark {

  @Param(Array("0", "1", "10", "1000", "10000"))
  var size: Int = _
  var integers: List[Int] = _
  var strings: List[String] = _
  var integersA: Array[Int] = _
  var stringsA: Array[String] = _


  @Setup(Level.Trial) def initNumbers: Unit = {
    integers = (1 to size).toList
    strings = integers.map(_.toString)
    integersA = integers.toArray
    stringsA = strings.toArray
  }

  @Benchmark def appendInteger(bh: Blackhole): Unit = {
    var arr = Array.empty[Int]
    integers foreach { i =>
      arr = arr.:+(i)
    }
    bh.consume(arr)
  }

  @Benchmark def appendString(bh: Blackhole): Unit = {
    var arr = Array.empty[String]
    strings foreach { i =>
      arr = arr.:+(i)
    }
    bh.consume(arr)
  }

  @Benchmark def insertInteger(bh: Blackhole): Unit = {
    var arr = Array.empty[Int]
    integers foreach { i =>
      arr = arr.+:(i)
    }
    bh.consume(arr)
  }

  @Benchmark def insertString(bh: Blackhole): Unit = {
    var arr = Array.empty[String]
    strings foreach { i =>
      arr = arr.+:(i)
    }
    bh.consume(arr)
  }

  @Benchmark def foldLeftSum(bh: Blackhole): Unit = {
    bh.consume(integersA.foldLeft(0){ (z,n) => z + n })
  }

  @Benchmark def foldSum(bh: Blackhole): Unit = {
    bh.consume(integersA.fold(0){ (a,b) => a + b })
  }

  @Benchmark def sortedStringOld(bh: Blackhole): Unit =
    bh.consume(oldSorted(stringsA))

  @Benchmark def sortedIntOld(bh: Blackhole): Unit =
    bh.consume(oldSorted(integersA))

  @Benchmark def sortedIntCustomOld(bh: Blackhole): Unit =
    bh.consume(oldSorted(integersA)(Ordering.Int.reverse))

  @Benchmark def sortedStringNew(bh: Blackhole): Unit =
    bh.consume(stringsA.sorted)

  @Benchmark def sortedIntNew(bh: Blackhole): Unit =
    bh.consume(integersA.sorted)

  @Benchmark def sortedIntCustomNew(bh: Blackhole): Unit =
    bh.consume(integersA.sorted(Ordering.Int.reverse))

  def oldSorted[A, B >: A](xs: Array[A])(implicit ord: Ordering[B]): Array[A] = {
    implicit def ct = ClassTag[A](xs.getClass.getComponentType)
    val len = xs.length
    if(xs.getClass.getComponentType.isPrimitive && len > 1) {
      // need to copy into a boxed representation to use Java's Arrays.sort
      val a = new Array[AnyRef](len)
      var i = 0
      while(i < len) {
        a(i) = xs(i).asInstanceOf[AnyRef]
        i += 1
      }
      Arrays.sort(a, ord.asInstanceOf[Ordering[AnyRef]])
      val res = new Array[A](len)
      i = 0
      while(i < len) {
        res(i) = a(i).asInstanceOf[A]
        i += 1
      }
      res
    } else {
      val copy = xs.slice(0, len)
      if(len > 1)
        Arrays.sort(copy.asInstanceOf[Array[AnyRef]], ord.asInstanceOf[Ordering[AnyRef]])
      copy
    }
  }
}
