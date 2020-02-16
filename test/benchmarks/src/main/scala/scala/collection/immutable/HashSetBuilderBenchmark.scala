package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

//typically run with
// bench/jmh:run scala.collection.immutable.HashSetBuilder -prof gc -rf csv

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
abstract class HashSetBuilderBaseBenchmark {
  @Param(Array(
    "10",
    "100",
    "1000",
    "10000"))
  var size: Int = _
  @Param(Array("true", "false"))
  var colliding: Boolean = _

  @Param(Array("Set+=", "Set++=", "HashSet+=", "HashSet++="))
  var op: String = _
  var operation: (Blackhole, Set[HashSetBenchmarkData], Set[HashSetBenchmarkData]) => Any = _

  // base data of specified size. All values are distinct
  var baseData: Array[HashSet[HashSetBenchmarkData]] = _
  // overlap(i) contains baseData(i) .. baseData(i+9) but with no structural sharing
  var overlap: Array[HashSet[HashSetBenchmarkData]] = _
  // overlap2(i) contains the same data as overlap(i) but with no structural sharing
  var overlap2: Array[HashSet[HashSetBenchmarkData]] = _
  // shared(i) contains baseData(i) .. baseData(i+9) but with structural sharing, both to the base data and preceding/subsequent entries
  var shared: Array[HashSet[HashSetBenchmarkData]] = _


  @Setup(Level.Trial) def initKeys(): Unit = {
    operation = op match {
      case "Set+=" => operationSetPlusEquals
      case "Set++=" => operationSetPlusPlusEquals
      case "HashSet+=" => operationHashSetPlusEquals
      case "HashSet++=" => operationHashSetPlusPlusEquals
    }

    def generate(prefix: String, size: Int) = {
      Array.tabulate(30)(i => (0 until size).map { k =>
        val data = s"key $i $k"
        val hash = if (colliding) (k >> 2) * i else data.hashCode
        HashSetBenchmarkData(hash, data)
      }(scala.collection.breakOut): HashSet[HashSetBenchmarkData])
    }

    baseData = generate("", size)

    overlap = new Array[HashSet[HashSetBenchmarkData]](baseData.length - 10)
    overlap2 = new Array[HashSet[HashSetBenchmarkData]](baseData.length - 10)
    shared = new Array[HashSet[HashSetBenchmarkData]](baseData.length - 10)
    for (i <- 0 until baseData.length - 10) {
      var s1: HashSet[HashSetBenchmarkData] = HashSet.empty[HashSetBenchmarkData]
      var s2: HashSet[HashSetBenchmarkData] = HashSet.empty[HashSetBenchmarkData]
      for (j <- i until i + 10) {
        baseData(j) foreach {
          x =>
            s1 += x
            s2 += x
        }
      }
      overlap(i) = s1
      overlap2(i) = s2
    }

    def base(i: Int) = {
      baseData(if (i < 0) baseData.length + i else i)
    }

    shared(0) = (-10 to(0, 1)).foldLeft(base(-10)) { case (a, b) => a ++ base(b) }
    for (i <- 1 until shared.length) {
      shared(i) = shared(i - 1) -- base(i - 10) ++ base(i)
    }

  }

  def operationSetPlusEquals(bh: Blackhole, set1: Set[HashSetBenchmarkData], set2: Set[HashSetBenchmarkData]) = {
    val builder = Set.newBuilder[HashSetBenchmarkData]
    builder ++= set1
    set2 foreach {
      builder += _
    }
    bh.consume(builder.result)
  }

  def operationSetPlusPlusEquals(bh: Blackhole, set1: Set[HashSetBenchmarkData], set2: Set[HashSetBenchmarkData]) = {
    val builder = Set.newBuilder[HashSetBenchmarkData]
    builder ++= set1
    builder ++= set2
    bh.consume(builder.result)
  }

  def operationHashSetPlusEquals(bh: Blackhole, set1: Set[HashSetBenchmarkData], set2: Set[HashSetBenchmarkData]) = {
    val builder = Set.newBuilder[HashSetBenchmarkData]
    builder ++= set1
    set2 foreach {
      builder += _
    }
    bh.consume(builder.result)
  }

  def operationHashSetPlusPlusEquals(bh: Blackhole, set1: Set[HashSetBenchmarkData], set2: Set[HashSetBenchmarkData]) = {
    val builder = Set.newBuilder[HashSetBenchmarkData]
    builder ++= set1
    builder ++= set2
    bh.consume(builder.result)
  }
}

class HashSetBuilderUnsharedBenchmark extends HashSetBuilderBaseBenchmark {

  @OperationsPerInvocation(30)
  @Benchmark def opDataWithEmpty(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 30) {
      operation(bh, baseData(i), HashSet.empty)
      i += 1
    }
  }

  @OperationsPerInvocation(30)
  @Benchmark def opEmptyWithData(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 30) {
      operation(bh, HashSet.empty, baseData(i))
      i += 1
    }
  }

  @OperationsPerInvocation(30)
  @Benchmark def opDataWithSetEmpty(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 30) {
      operation(bh, baseData(i), Set.empty)
      i += 1
    }
  }

  @OperationsPerInvocation(30)
  @Benchmark def opSetEmptyWithData(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 30) {
      operation(bh, Set.empty, baseData(i))
      i += 1
    }
  }

  @OperationsPerInvocation(29)
  @Benchmark def opWithDistinct(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 29) {
      operation(bh, baseData(i), baseData(i + 1))
      i += 1
    }
  }

  @OperationsPerInvocation(20)
  @Benchmark def opDataWithContainedUnshared(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 20) {
      operation(bh, overlap(i), baseData(i))
      i += 1
    }
  }

  @OperationsPerInvocation(20)
  @Benchmark def opDataWithContainedShared(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 20) {
      operation(bh, shared(i), baseData(i))
      i += 1
    }
  }

  @OperationsPerInvocation(20)
  @Benchmark def opContainedUnsharedWithData(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 20) {
      operation(bh, baseData(i), overlap(i))
      i += 1
    }
  }

  @OperationsPerInvocation(20)
  @Benchmark def opContainedSharedWithData(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 20) {
      operation(bh, baseData(i), shared(i))
      i += 1
    }
  }
}

class HashSetBuilderSharedBenchmark extends HashSetBuilderBaseBenchmark {
  @Param(Array("0", "20", "40", "60", "80", "90", "100"))
  var sharing: Int = _

  @OperationsPerInvocation(10)
  @Benchmark def opWithOverlapUnshared(bh: Blackhole): Unit = {
    var i = 10;
    while (i < 20) {
      operation(bh, overlap(i - (10 - sharing / 10)), overlap2(i))
      i += 1
    }
  }

  @OperationsPerInvocation(10)
  @Benchmark def opWithOverlapShared(bh: Blackhole): Unit = {
    var i = 10;
    while (i < 20) {
      operation(bh, shared(i - (10 - sharing / 10)), shared(i))
      i += 1
    }
  }
}


//for testing, debugging, optimising etc
object TestHashSetBenchmark extends App {

  val bh = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.")
  val test = new HashSetBuilderUnsharedBenchmark

  test.size = 10000
  test.op = "++"
  test.colliding = true
  test.initKeys()
  while (true) {
    var j = 0
    val start = System.nanoTime()
    while (j < 100) {
      test.opDataWithContainedUnshared(bh)
      j += 1
    }
    val end = System.nanoTime()
    println((end - start) / 1000000)
  }
}
