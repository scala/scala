package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

//typical usage bench/jmh:run scala.collection.immutable.TreeSetBenchmark --prof gc

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class TreeSetBenchmark {

  val small =  (Array.tabulate(10) (_.toString)).to[TreeSet]
  val smallHash: HashSet[String] = HashSet.empty ++ small
  val rawData = Array.tabulate(1000) (_.toString)

  val large = rawData.to[TreeSet]
  val largeHash:  HashSet[String] = HashSet.empty ++ rawData
  val large2 = large map (_+ "-xx")
  val large2Hash:  HashSet[String] = HashSet.empty ++ large2

  val one =  TreeSet[String] ("f")

  @Benchmark def plusPlus(bh: Blackhole): Unit = {
    bh.consume(large ++ large2)
  }
  @OperationsPerInvocation(1000)
  @Benchmark def builderPlus(bh: Blackhole): Unit = {
    val builder = TreeSet.newBuilder[String]
    var i = 0
    while (i < 1000) {
      builder += rawData(i)
      i += 1
    }
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusInitial(bh: Blackhole): Unit = {
    val builder = TreeSet.newBuilder[String]
    builder ++= large
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusInitialHash(bh: Blackhole): Unit = {
    val builder = TreeSet.newBuilder[String]
    builder ++= largeHash
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusSame(bh: Blackhole): Unit = {
    val builder = TreeSet.newBuilder[String]
    builder ++= large
    builder ++= large
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusSameHash(bh: Blackhole): Unit = {
    val builder = TreeSet.newBuilder[String]
    builder ++= large
    builder ++= largeHash
    bh.consume(builder.result)
  }

  @Benchmark def builderPlusPlusLargeLarge(bh: Blackhole): Unit = {
    val builder = TreeSet.newBuilder[String]
    builder ++= large
    builder ++= large2
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusLargeLargeHash(bh: Blackhole): Unit = {
    val builder = TreeSet.newBuilder[String]
    builder ++= large
    builder ++= large2Hash
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusSmallLarge(bh: Blackhole): Unit = {
    val builder = TreeSet.newBuilder[String]
    builder ++= small
    builder ++= large
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusSmallLargeHash(bh: Blackhole): Unit = {
    val builder = TreeSet.newBuilder[String]
    builder ++= small
    builder ++= largeHash
    bh.consume(builder.result)
  }
  @Benchmark def builderPlusPlusLargeSmall(bh: Blackhole): Unit = {
    val builder = TreeSet.newBuilder[String]
    builder ++= large
    builder ++= small
    bh.consume(builder.result)
  }

}

//for testing, debugging, optimising etc
object TreeSetTest extends App {

  val bh = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.")
  val test = new TreeSetBenchmark

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
