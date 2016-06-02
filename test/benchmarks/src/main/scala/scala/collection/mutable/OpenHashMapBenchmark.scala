package scala.collection.mutable;

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import org.openjdk.jol.info.GraphLayout

import benchmark._
import java.util.concurrent.TimeUnit

/** Utilities for the [[OpenHashMapBenchmark]].
  * 
  * The method calls are tested by looping to the size desired for the map;
  * instead of using the JMH harness, which iterates for a fixed length of time.
  */
private object OpenHashMapBenchmark {

  /** Abstract state container for the `put()` bulk calling tests.
    * 
    * Provides an array of adequately-sized, empty maps to each invocation,
    * so that hash table allocation won't be done during measurement.
    * Provides enough maps to make each invocation long enough to avoid timing artifacts.
    * Performs a GC after re-creating the empty maps before every invocation,
    * so that only the GCs caused by the invocation contribute to the measurement.
    * 
    * Records the memory used by all the maps in the last invocation of each iteration.
    * 
    * @tparam K type of the map keys to be used in the test
    */
  @State(Scope.Thread)
  private[this] abstract class BulkPutState[K](implicit keyBuilder: KeySeqBuilder[K]) {
    /** A lower-bound estimate of the number of nanoseconds per `put()` call */
    private[this] val nanosPerPut: Double = 5

    /** Minimum number of nanoseconds per invocation, so as to avoid timing artifacts. */
    private[this] val minNanosPerInvocation = 1000000  // one millisecond

    /** Size of the maps created in this trial. */
    private[this] var size: Int = _

    /** Total number of entries in all of the `maps` combined. */
    private[this] var _mapEntries: Int = _
    protected def mapEntries = _mapEntries

    /** Number of operations performed in the current invocation. */
    private[this] var _operations: Int = _
    protected def operations = _operations

    /** Bytes of memory used in the object graphs of all the maps. */
    private[this] var _memory: Long = _
    protected def memory = _memory

    /** The sequence of keys to store into a map. */
    private[this] var _keys: KeySeq[K] = _
    def keys() = _keys

    var maps: Array[OpenHashMap[K,Int]] = null

    @Setup
    def threadSetup(params: BenchmarkParams) {
      size = params.getParam("size").toInt
      val n = math.ceil(minNanosPerInvocation / (nanosPerPut * size)).toInt
      _mapEntries = size * n
      _keys = keyBuilder.build(size)
      maps = new Array(n)
    }

    @Setup(Level.Iteration)
    def iterationSetup {
      _operations = 0
    }

    @Setup(Level.Invocation)
    def setup(params: IterationParams) {
      for (i <- 0 until maps.length) maps(i) = new OpenHashMap[K,Int](size)

      if (params.getType == IterationType.MEASUREMENT) {
        _operations += _mapEntries
        System.gc()  // clean up after last invocation
      }
    }

    @TearDown(Level.Iteration)
    def iterationTeardown(params: IterationParams) {
      if (params.getType == IterationType.MEASUREMENT) {
        // limit to smaller cases to avoid OOM
        _memory =
          if (_mapEntries <= 1000000) GraphLayout.parseInstance(maps(0), maps.tail).totalSize
          else 0
      }
    }
  }

  /** Abstract state container for the `get()` bulk calling tests.
    * 
    * Provides a thread-scoped map of the expected size.
    * Performs a GC after loading the map.
    * 
    * @tparam K type of the map keys to be used in the test
    */
  @State(Scope.Thread)
  private[this] abstract class BulkGetState[K](implicit keyBuilder: KeySeqBuilder[K]) {
    /** The sequence of keys to store into a map. */
    private[this] var _keys: KeySeq[K] = _
    def keys() = _keys

    val map = new OpenHashMap[K,Int].empty

    /** Load the map with keys from `1` to `size`. */
    @Setup
    def setup(params: BenchmarkParams) {
      val size = params.getParam("size").toInt
      _keys = keyBuilder.build(size)
      put(map, keys, 0, size)
      System.gc()
    }
  }

  /** Abstract state container for the `get()` bulk calling tests with deleted entries.
    * 
    * Provides a thread-scoped map of the expected size, from which entries have been removed.
    * Performs a GC after loading the map.
    * 
    * @tparam K type of the map keys to be used in the test
    */
  @State(Scope.Thread)
  private[this] abstract class BulkRemovedGetState[K](implicit keyBuilder: KeySeqBuilder[K]) {
    /** The sequence of keys to store into a map. */
    private[this] var _keys: KeySeq[K] = _
    def keys() = _keys

    val map = new OpenHashMap[K,Int].empty

    /** Load the map with keys from `1` to `size`, removing half of them. */
    @Setup
    def setup(params: BenchmarkParams) {
      val size = params.getParam("size").toInt
      _keys = keyBuilder.build(size)
      put_remove(map, keys)
      System.gc()
    }
  }

  /* In order to use `@AuxCounters` on a class hierarchy (as of JMH 1.11.3),
   * it's necessary to place it on the injected (sub)class, and to make the
   * counters visible as explicit public members of the that class.  JMH doesn't
   * scan the ancestor classes for counters.
   */

  @AuxCounters
  private class IntBulkPutState extends BulkPutState[Int] {
    override def mapEntries = super.mapEntries
    override def operations = super.operations
    override def memory = super.memory
  }
  private class IntBulkGetState extends BulkGetState[Int]
  private class IntBulkRemovedGetState extends BulkRemovedGetState[Int]

  @AuxCounters
  private class AnyRefBulkPutState extends BulkPutState[AnyRef] {
    override def mapEntries = super.mapEntries
    override def operations = super.operations
    override def memory = super.memory
  }
  private class AnyRefBulkGetState extends BulkGetState[AnyRef]
  private class AnyRefBulkRemovedGetState extends BulkRemovedGetState[AnyRef]


  /** Put entries into the given map.
    * Adds entries using a range of keys from the given list.
    * 
    * @param from lowest index in the range of keys to add
    * @param to highest index in the range of keys to add, plus one
    */
  private[this] def put[K](map: OpenHashMap[K,Int], keys: KeySeq[K], from: Int, to: Int) {
    var i = from
    while (i < to) {  // using a `for` expression instead adds significant overhead
      map.put(keys(i), i)
      i += 1
    }
  }

  /** Put entries into the given map.
    * Adds entries using all of the keys from the given list.
    */
  private def put[K](map: OpenHashMap[K,Int], keys: KeySeq[K]): Unit =
    put(map, keys, 0, keys.size)

  /** Put entries into the given map, removing half of them as they're added.
    * 
    * @param keys list of keys to use
    */
  private def put_remove[K](map: OpenHashMap[K,Int], keys: KeySeq[K]) {
    val blocks = 25  // should be a non-trivial factor of `size`
    val size = keys.size
    val blockSize: Int = size / blocks
    var base = 0
    while (base < size) {
      put(map, keys, base, base + blockSize)

      // remove every other entry
      var i = base
      while (i < base + blockSize) {
        map.remove(keys(i))
        i += 2
      }

      base += blockSize
    }
  }

  /** Get elements from the given map. */
  private def get[K](map: OpenHashMap[K,Int], keys: KeySeq[K]) = {
    val size = keys.size
    var i = 0
    var sum = 0
    while (i < size) {
      sum += map.get(keys(i)).getOrElse(0)
      i += 1
    }
    sum
  }
}

/** Benchmark for the library's [[OpenHashMap]]. */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(5)
@Threads(1)
@Warmup(iterations = 20)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class OpenHashMapBenchmark {
  import OpenHashMapBenchmark._

  @Param(Array("50", "100", "1000", "10000", "100000", "1000000", "2500000",
               "5000000", "7500000", "10000000", "25000000"))
  var size: Int = _

  // Tests with Int keys

  /** Test putting elements to a map of `Int` to `Int`. */
  @Benchmark
  def put_Int(state: IntBulkPutState) {
    var i = 0
    while (i < state.maps.length) {
      put(state.maps(i), state.keys)
      i += 1
    }
  }

  /** Test putting and removing elements to a growing map of `Int` to `Int`. */
  @Benchmark
  def put_remove_Int(state: IntBulkPutState) {
    var i = 0
    while (i < state.maps.length) {
      put_remove(state.maps(i), state.keys)
      i += 1
    }
  }

  /** Test getting elements from a map of `Int` to `Int`. */
  @Benchmark
  def get_Int_after_put(state: IntBulkGetState) =
    get(state.map, state.keys)

  /** Test getting elements from a map of `Int` to `Int` from which elements have been removed.
    * Note that half of these queries will fail to find their keys, which have been removed.
    */
  @Benchmark
  def get_Int_after_put_remove(state: IntBulkRemovedGetState) =
    get(state.map, state.keys)


  // Tests with AnyRef keys

  /** Test putting elements to a map of `AnyRef` to `Int`. */
  @Benchmark
  def put_AnyRef(state: AnyRefBulkPutState) {
    var i = 0
    while (i < state.maps.length) {
      put(state.maps(i), state.keys)
      i += 1
    }
  }

  /** Test putting and removing elements to a growing map of `AnyRef` to `Int`. */
  @Benchmark
  def put_remove_AnyRef(state: AnyRefBulkPutState) {
    var i = 0
    while (i < state.maps.length) {
      put_remove(state.maps(i), state.keys)
      i += 1
    }
  }

  /** Test getting elements from a map of `AnyRef` to `Int`. */
  @Benchmark
  def get_AnyRef_after_put(state: AnyRefBulkGetState) =
    get(state.map, state.keys)

  /** Test getting elements from a map of `AnyRef` to `Int` from which elements have been removed.
    * Note that half of these queries will fail to find their keys, which have been removed.
    */
  @Benchmark
  def get_AnyRef_after_put_remove(state: AnyRefBulkRemovedGetState) =
    get(state.map, state.keys)
}
