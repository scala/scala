package scala.collection.mutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ArrayOpsBenchmark {

  @Param(Array("10", "1000", "10000"))
  var size: Int = _
  var integers: List[Int] = _
  var strings: List[String] = _


  @Setup(Level.Trial) def initNumbers: Unit = {
    integers = (1 to size).toList
    strings = integers.map(_.toString)
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
}
