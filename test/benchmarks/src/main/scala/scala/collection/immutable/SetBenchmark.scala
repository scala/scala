package scala.collection.immutable

import java.util.concurrent.TimeUnit

import benchmark._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

private object SetBenchmark {

  @State(Scope.Thread)
  private[this] abstract class DiffByFilterBulk[T](implicit keyBuilder: TwoSetInRatioProvider[T]) {

    /** Total number of values in both sets. */
    private[this] var size: Int = _

    /** Target proportion of sizes of both sets. */
    private[this] var ratio: Float = _

    /** Target share of values that are in both sets. */
    private[this] var intersection: Float = _

    /** The set from which we want to subtract another set */
    private[this] var _set1: Set[T] = _
    def set1 = _set1

    /** The set values of which we do not desire in set1 */
    private[this] var _set2: Set[T] = _
    def set2 = _set2

    /** Build both sets. */
    @Setup
    def threadSetup(params: BenchmarkParams) {
      size = params.getParam("size").toInt
      ratio = params.getParam("ratio").toFloat
      intersection = params.getParam("intersection").toFloat

      val generatedSets: (Predef.Set[T], Predef.Set[T]) = keyBuilder.build(size, ratio, intersection)
      _set1 = generatedSets._1.toSet
      _set2 = generatedSets._2.toSet
      System.gc()
    }
  }

  private class IntDiffByFilterBulk extends DiffByFilterBulk[Int]

}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 2)
@Measurement(iterations = 2)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class SetBenchmark {

  import SetBenchmark._

  @Param(Array("100", "1000", "100000", "10000000"))
  var size: Int = _

  @Param(Array("0.0", "0.1", "0.25", "0.5", "0.75", "0.9", "1.0"))
  var ratio: Float = _

  @Param(Array("0.0", "0.33", "0.66", "1.0"))
  var intersection: Float = _

  // Tests with Int values

  @Benchmark
  def int_set_diff_by_filtering(state: IntDiffByFilterBulk) = state.set1 filterNot state.set2

  /** Be sure You run this benchmark with a version of scala where &~ (diff) is implemented as -- **/
  @Benchmark
  def int_set_diff_by_removing(state: IntDiffByFilterBulk) = state.set1 -- state.set2

  @Benchmark
  def int_set_diff_by_diff(state: IntDiffByFilterBulk) = state.set1 &~ state.set2

}

