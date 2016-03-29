package scala.collection.mutable;

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.infra.BenchmarkParams
import org.openjdk.jol.info.GraphLayout
import org.openjdk.jol.info.GraphWalker
import org.openjdk.jol.info.GraphVisitor
import org.openjdk.jmh.infra.IterationParams
import org.openjdk.jmh.runner.IterationType

/** Utilities for the [[OpenHashMapBenchmark]].
  * 
  * The method calls are tested by looping to the size desired for the map;
  * instead of using the JMH harness, which iterates for a fixed length of time.
  */
private object OpenHashMapBenchmark {
  /** State container for the `put()` bulk calling tests.
    * 
    * Provides an array of adequately-sized, empty maps to each invocation,
    * so that hash table allocation won't be done during measurement.
    * Provides enough maps to make each invocation long enough to avoid timing artifacts.
    * Performs a GC after re-creating the empty maps before every invocation,
    * so that only the GCs caused by the invocation contribute to the measurement.
    * 
    * Records the memory used by all the maps in the last invocation of each iteration.
    */
  @State(Scope.Thread)
  @AuxCounters
  class BulkPutState {
    /** A lower-bound estimate of the number of nanoseconds per `put()` call */
    private[this] val nanosPerPut: Double = 5

    /** Minimum number of nanoseconds per invocation, so as to avoid timing artifacts. */
    private[this] val minNanosPerInvocation = 1000000  // one millisecond

    /** Size of the maps created in this trial. */
    private[this] var size: Int = _

    /** Total number of entries in all of the `maps` combined. */
    var mapEntries: Int = _

    /** Number of operations performed in the current invocation. */
    var operations: Int = _

    /** Bytes of memory used in the object graphs of all the maps. */
    var memory: Long = _

    var maps: Array[OpenHashMap[Int,Int]] = null

    @Setup
    def threadSetup(params: BenchmarkParams) {
      size = params.getParam("size").toInt
      val n = math.ceil(minNanosPerInvocation / (nanosPerPut * size)).toInt
      mapEntries = size * n
      maps = new Array(n)
    }

    @Setup(Level.Iteration)
    def iterationSetup {
      operations = 0
    }

    @Setup(Level.Invocation)
    def setup(params: IterationParams) {
      for (i <- 0 until maps.length) maps(i) = new OpenHashMap[Int,Int](size)

      if (params.getType == IterationType.MEASUREMENT) {
        operations += mapEntries
        System.gc()  // clean up after last invocation
      }
    }

    @TearDown(Level.Iteration)
    def iterationTeardown(params: IterationParams) {
      if (params.getType == IterationType.MEASUREMENT) {
        // limit to smaller cases to avoid OOM
        memory = if (mapEntries <= 1000000) GraphLayout.parseInstance(maps(0), maps.tail).totalSize else 0
      }
    }
  }

  /** State container for the `get()` bulk calling tests.
    * 
    * Provides a thread-scoped map of the expected size.
    * Performs a GC after loading the map.
    */
  @State(Scope.Thread)
  class BulkGetState {
    val map = new OpenHashMap[Int,Int].empty

    /** Load the map with keys from `1` to `size`. */
    @Setup
    def setup(params: BenchmarkParams) {
      val size = params.getParam("size").toInt
      put_Int(map, 1, size)
      System.gc()
    }
  }

  /** State container for the `get()` bulk calling tests with deleted entries.
    * 
    * Provides a thread-scoped map of the expected size, from which entries have been removed.
    * Performs a GC after loading the map.
    */
  @State(Scope.Thread)
  class BulkRemovedGetState {
    val map = new OpenHashMap[Int,Int].empty

    /** Load the map with keys from `1` to `size`, removing half of them. */
    @Setup
    def setup(params: BenchmarkParams) {
      val size = params.getParam("size").toInt
      put_remove_Int(map, size)
      System.gc()
    }
  }

  /** Put elements into the given map. */
  private def put_Int(map: OpenHashMap[Int,Int], from: Int, to: Int) {
    var i = from
    while (i <= to) {  // using a `for` expression instead adds significant overhead
      map.put(i, i)
      i += 1
    }
  }

  /** Put elements into the given map, removing half of them as they're added.
    * 
    * @param size number of entries to leave in the map on return
    */
  def put_remove_Int(map: OpenHashMap[Int,Int], size: Int) {
    val blocks = 50  // should be a factor of `size`
    val totalPuts = 2 * size  // add twice as many, because we remove half of them
    val blockSize: Int = totalPuts / blocks
    var base = 0
    while (base < totalPuts) {
      put_Int(map, base + 1, base + blockSize)

      // remove every other entry
      var i = base + 1
      while (i <= base + blockSize) {
        map.remove(i)
        i += 2
      }

      base += blockSize
    }
  }

  /** Get elements from the given map. */
  def get_Int(map: OpenHashMap[Int,Int], size: Int, bh: Blackhole) {
    var i = 1
    while (i <= size) {
      bh.consume(map.get(i).getOrElse(0))
      i += 1
    }
  }
}

/** Benchmark for the library's [[OpenHashMap]]. */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(6)
@Threads(1)
@Warmup(iterations = 20)
@Measurement(iterations = 6)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class OpenHashMapBenchmark {
  import OpenHashMapBenchmark._

  @Param(Array("50", "100", "250", "1000", "2500", "10000", "25000", "100000", "250000", "1000000", "2500000",
               "5000000", "7500000", "10000000", "25000000"))
  var size: Int = _

  /** Test putting elements to a map of `Int` to `Int`. */
  @Benchmark
  def put_Int(state: BulkPutState) {
    var i = 0
    while (i < state.maps.length) {
      OpenHashMapBenchmark.put_Int(state.maps(i), 1, size)
      i += 1
    }
  }

  /** Test putting and removing elements to a growing map of `Int` to `Int`. */
  @Benchmark
  def put_remove_Int(state: BulkPutState) {
    var i = 0
    while (i < state.maps.length) {
      OpenHashMapBenchmark.put_remove_Int(state.maps(i), size)
      i += 1
    }
  }

  /** Test getting elements from a map of `Int` to `Int`. */
  @Benchmark
  def put_get_Int(state: BulkGetState, bh: Blackhole) = OpenHashMapBenchmark.get_Int(state.map, size, bh)

  /** Test getting elements from a map of `Int` to `Int` from which elements have been removed. */
  @Benchmark
  def put_remove_get_Int(state: BulkRemovedGetState, bh: Blackhole) = OpenHashMapBenchmark.get_Int(state.map, size, bh)
}
