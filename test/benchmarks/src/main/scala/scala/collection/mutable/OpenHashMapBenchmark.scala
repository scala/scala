package scala.collection.mutable;

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

private object OpenHashMapBenchmark {
  /** State container for the `put()` bulk calling tests.
    * 
    * Provides a thread-scoped map, so that allocation for the hash table will be done
    * in the first warm-up iteration, not during measurement.
    * 
    * Performs a GC after every invocation, so that only the GCs caused by the invocation
    * contribute to the measurement.
    */
  @State(Scope.Thread)
  class BulkPutState {
    val map = new OpenHashMap[Int,Int].empty
  
    @TearDown(Level.Invocation)
    def teardown { map.clear(); System.gc() }
  }
}

/** Benchmark for the library's [[OpenHashMap]].
  * 
  * The `put()` calls are tested by looping to the size desired for the map;
  * instead of using the JMH harness, which iterates for a fixed length of time.
  */
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 20)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class OpenHashMapBenchmark {
  import OpenHashMapBenchmark._

  @Param(Array("100", "250", "1000", "2500", "10000", "25000", "100000", "250000", "1000000", "2500000",
               "5000000", "7500000", "10000000", "25000000"))
  var size: Int = _

  /** Put elements into the given map. */
  private[this] def put_Int(map: OpenHashMap[Int,Int], from: Int, to: Int) {
    var i = from
    while (i <= to) {  // using a `for` expression instead adds significant overhead
      map.put(i, i)
      i += 1
    }
  }

  /** Test putting elements to a map of `Int` to `Int`. */
  @Benchmark
  def put_Int(state: BulkPutState) { put_Int(state.map, 1, size) }

  /** Test putting and removing elements to a growing map of `Int` to `Int`. */
  @Benchmark
  def put_remove_Int(state: BulkPutState) {
    val blocks = 50  // should be a factor of `size`
    val totalPuts = 2 * size  // add twice as many, because we remove half of them
    val blockSize: Int = totalPuts / blocks
    var base = 0
    while (base < totalPuts) {
      put_Int(state.map, base + 1, base + blockSize)

      // remove every other entry
      var i = base + 1
      while (i <= base + blockSize) {
        state.map.remove(i)
        i += 2
      }

      base += blockSize
    }
  }
}
