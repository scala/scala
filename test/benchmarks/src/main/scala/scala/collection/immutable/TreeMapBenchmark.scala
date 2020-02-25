package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
//typical usage bench/jmh:run scala.collection.immutable.TreeMapBenchmark --prof gc

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class TreeMapBenchmark {

  val small = TreeMap.empty[String, String] ++ (Array.tabulate(10)(x => x.toString -> x.toString))
  val smallHash: HashMap[String, String] = (HashMap.newBuilder[String, String] ++= small).result
  val rawData = Array.tabulate(1000)(x => x.toString -> x.toString)

  val large: TreeMap[String, String] = TreeMap.empty[String, String] ++ rawData
  val largeHash: HashMap[String, String] =  (HashMap.newBuilder[String, String] ++= rawData).result
  val largeDifferentValues: TreeMap[String, String] = large map { case ((k, v)) => k -> (v + "-xx") }
  val largeDifferentValuesHash: HashMap[String, String] =  (HashMap.newBuilder[String, String]  ++= largeDifferentValues).result
  val large2: TreeMap[String, String] = large.map { case ((k, v)) => (k + "-yy") -> (v + "-xx") }
  val large2Hash: HashMap[String, String] =  (HashMap.newBuilder[String, String] ++= large2).result

  val one = TreeMap[String, String]("a" -> "b")

  @Benchmark def plusPlus(bh: Blackhole): Unit = {
    bh.consume(large ++ large2)
  }

  @OperationsPerInvocation(1000)
  @Benchmark def builderPlus(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    var i = 0
    while (i < 1000) {
      builder += rawData(i)
      i += 1
    }
    bh.consume(builder.result)
  }

  @Benchmark def builderPlusPlusInitial(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    builder ++= large
    bh.consume(builder.result)
  }

  @Benchmark def builderPlusPlusInitialHash(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    builder ++= large2Hash
    bh.consume(builder.result)
  }

  @Benchmark def builderPlusPlusSame(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    builder ++= large
    builder ++= large
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusSameHash(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    builder ++= large
    builder ++= largeHash
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusDifferntValues(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    builder ++= large
    builder ++= largeDifferentValues
    builder ++= large
    bh.consume(builder.result)
  }

  @Benchmark def builderPlusPlusLargeLarge(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    builder ++= large
    builder ++= large2
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusLargeLargeHash(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    builder ++= large
    builder ++= large2Hash
    bh.consume(builder.result)
  }

  @Benchmark def builderPlusPlusSmallLarge(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    builder ++= small
    builder ++= large
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusSmallLargeHash(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    builder ++= small
    builder ++= largeHash
    bh.consume(builder.result)
  }

  @Benchmark def builderPlusPlusLargeSmall(bh: Blackhole): Unit = {
    val builder = TreeMap.newBuilder[String, String]
    builder ++= large
    builder ++= small
    bh.consume(builder.result)
  }

}

//for testing, debugging, optimising etc
object TreeMapTest extends App {

  val bh = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.")
  val test = new TreeMapBenchmark

  while (true) {
    var j = 0
    val start = System.nanoTime()
    while (j < 100) {
      test.builderPlusPlusLargeSmall(bh)
      j += 1
    }
    val end = System.nanoTime()
    println((end - start) / 1000000)
  }
}
