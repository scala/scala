package scala.collection.mutable

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@Fork(1)
@Threads(1)
@Warmup(iterations = 4, time = 4, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 5, timeUnit = TimeUnit.SECONDS)
@State(Scope.Benchmark)
class ArrayIteratorBenchmarks {

  @Param(Array(
    "0",
    "100",
    /*"200",
    "300",
    "400",
    "500",
    "600",
    "700",
    "800",
    "900",*/
    "1000",
    "10000",
    "100000",
    "1000000",
    "10000000",
    "100000000",
  ))
  var valueCount: Int = _

  var values: Array[Int] = _

  @Setup
  def setValues(): Unit = {
    val random: util.Random = new util.Random(0)
    values = Array.fill(valueCount)(random.nextInt())
  }

  @Benchmark
  def arrayIterator(blackhole: Blackhole): Unit = {
    val i = values.iterator
    while (i.hasNext) blackhole.consume(i.next())
  }
}
