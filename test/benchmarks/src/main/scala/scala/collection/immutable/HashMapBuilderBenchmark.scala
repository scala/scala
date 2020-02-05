package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

//typically run with
// bench/jmh:run scala.collection.immutable. HashMapBuilder --prof gc --rf csv

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
abstract class HashMapBuilderBenchmark {
  @Param(Array(
    "10",
    "100",
    "1000",
    "10000"
  ))
  var size: Int = _
  @Param(Array("true", "false"))
  var colliding: Boolean = _

  @Param(Array("Map+=", "Map++=", "HashMap+=", "HashMap++="))
  var op: String = _
  var operation: (Blackhole, Map[HashMapBenchmarkData, String], Map[HashMapBenchmarkData, String]) => Any = _

  // base data of specified size. All values are distinct
  var baseData: Array[HashMap[HashMapBenchmarkData, String]] = _
  // overlap(i) contains baseData(i) .. baseData(i+9) but with no structural sharing
  var overlap: Array[HashMap[HashMapBenchmarkData, String]] = _
  // overlap2(i) contains the same data as overlap(i) but with no structural sharing
  var overlap2: Array[HashMap[HashMapBenchmarkData, String]] = _
  // shared(i) contains baseData(i) .. baseData(i+9) but with structural sharing, both to the base data and preceding/subsequent entries
  var shared: Array[HashMap[HashMapBenchmarkData, String]] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    operation = op match {
      case "Map+=" => operationMapPlusEquals
      case "Map++=" => operationMapPlusPlusEquals
      case "HashMap+=" => operationHashMapPlusEquals
      case "HashMap++=" => operationHashMapPlusPlusEquals
    }

    def generate(prefix: String, size: Int) = {
      Array.tabulate(30)(i => (0 until size).map { k =>
        val data = s"key $i $k"
        val hash = if (colliding) (k >> 2) * i else data.hashCode
        HashMapBenchmarkData(hash, data) -> s"value $i $k"
      }(scala.collection.breakOut): HashMap[HashMapBenchmarkData, String])
    }

    baseData = generate("", size)

    overlap = new Array[HashMap[HashMapBenchmarkData, String]](baseData.length - 10)
    overlap2 = new Array[HashMap[HashMapBenchmarkData, String]](baseData.length - 10)
    shared = new Array[HashMap[HashMapBenchmarkData, String]](baseData.length - 10)
    for (i <- 0 until baseData.length - 10) {
      var s1 = HashMap.empty[HashMapBenchmarkData, String]
      var s2 = HashMap.empty[HashMapBenchmarkData, String];
      for (j <- 0 until 10) {
        baseData(j) foreach {
          x =>
            s1 += x
            s2 += x
        }
      }
      overlap(i) = s1
      overlap2(i) = s2

    }
    def base (i:Int) = {
      baseData(if (i < 0) baseData.length+i else i)
    }
    shared(0) = (-10 to (0, 1)).foldLeft (base(-10)) {case (a, b) => a ++ base(b)}
    for (i <- 1 until baseData.length - 10) {
      shared(i) = shared(i - 1) -- base(i - 10).keys ++ base(i)
    }
  }
  def operationMapPlusEquals(bh: Blackhole, map1: Map[HashMapBenchmarkData, String], map2: Map[HashMapBenchmarkData, String]) = {
    val builder = Map.newBuilder[HashMapBenchmarkData, String]
    builder ++= map1
    map2 foreach {
      builder += _
    }
    bh.consume(builder.result())
  }
  def operationHashMapPlusEquals(bh: Blackhole, map1: Map[HashMapBenchmarkData, String], map2: Map[HashMapBenchmarkData, String]) = {
    val builder = HashMap.newBuilder[HashMapBenchmarkData, String]
    builder ++= map1
    map2 foreach {
      builder += _
    }
    bh.consume(builder.result())
  }
  def operationMapPlusPlusEquals(bh: Blackhole, map1: Map[HashMapBenchmarkData, String], map2: Map[HashMapBenchmarkData, String]) = {
    val builder = Map.newBuilder[HashMapBenchmarkData, String]
    builder ++= map1
    builder ++= map2
    bh.consume(builder.result())
  }
  def operationHashMapPlusPlusEquals(bh: Blackhole, map1: Map[HashMapBenchmarkData, String], map2: Map[HashMapBenchmarkData, String]) = {
    val builder = HashMap.newBuilder[HashMapBenchmarkData, String]
    builder ++= map1
    builder ++= map2
    bh.consume(builder.result())
  }
}

class HashMapBuilderUnsharedBenchmark extends HashMapBuilderBenchmark {

  @OperationsPerInvocation(30)
  @Benchmark def opDataWithEmpty(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 30) {
      operation(bh, baseData(i), HashMap.empty)
      i += 1
    }
  }

  @OperationsPerInvocation(30)
  @Benchmark def opEmptyWithData(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 30) {
      operation(bh, HashMap.empty, baseData(i))
      i += 1
    }
  }
  @OperationsPerInvocation(30)
  @Benchmark def opDataWithMapEmpty(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 30) {
      operation(bh, baseData(i), Map.empty)
      i += 1
    }
  }

  @OperationsPerInvocation(30)
  @Benchmark def opMapEmptyWithData(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 30) {
      operation(bh, Map.empty, baseData(i))
      i += 1
    }
  }

  @OperationsPerInvocation(29)
  @Benchmark def opWithDistinct(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 29) {
      operation(bh, baseData(i), baseData(i+1))
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
      operation(bh,  baseData(i), overlap(i))
      i += 1
    }
  }

  @OperationsPerInvocation(20)
  @Benchmark def opContainedSharedWithData(bh: Blackhole): Unit = {
    var i = 0;
    while (i < 20) {
      operation(bh,  baseData(i), shared(i))
      i += 1
    }
  }
}
class HashMapBuilderSharedBenchmark extends HashMapBuilderBenchmark {
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
