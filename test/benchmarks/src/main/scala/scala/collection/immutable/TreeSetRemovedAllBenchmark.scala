package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.util.Random

/** `removedAll` with an argument that is not a `TreeSet` of the same ordering.
 *  Run with `-prof gc` to see the allocation per call.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class TreeSetRemovedAllBenchmark {

  @Param(Array("4096", "100000"))
  var size: Int = _

  /** How many elements of the set the argument holds: a number, `half` or `all` of the set, or a count around where
   *  `removedAll` changes method. It deletes the first `limit` = n / (2 log2 n) elements found one by one, keeps the
   *  next `3 * limit` in an array (`L+B` is `limit` plus that array's length), and filters the tree past that.
   */
  @Param(Array("1", "3", "10", "64", "L+1", "L+B", "L+B+1", "1.5(L+B)", "half", "all"))
  var removed: String = _

  var set: TreeSet[Int] = _
  var present: Vector[Int] = _     // elements of the set, in random order
  var presentList: List[Int] = _  // the same, in a List, whose size is not known in advance
  var absent: Vector[Int] = _      // as many elements that are not in the set

  @Setup(Level.Trial) def init(): Unit = {
    val limit = math.max(1, size / (2 * math.max(1, 32 - Integer.numberOfLeadingZeros(size))))
    val bound = 3 * limit
    val k = removed match {
      case "L+1"      => limit + 1
      case "L+B"      => limit + bound
      case "L+B+1"    => limit + bound + 1
      case "1.5(L+B)" => (limit + bound) * 3 / 2
      case "half"     => size / 2
      case "all"      => size
      case n          => n.toInt
    }
    val rnd = new Random(0)
    set = TreeSet.from((0 until size).map(_ * 2))
    present = rnd.shuffle((0 until size).toVector).take(k).map(_ * 2)
    presentList = present.toList
    absent = present.map(_ + 1)
  }

  @Benchmark
  def treeSet(bh: Blackhole): Unit = bh.consume(set.removedAll(present))

  @Benchmark
  def treeSetList(bh: Blackhole): Unit = bh.consume(set.removedAll(presentList))

  @Benchmark
  def treeSetAbsent(bh: Blackhole): Unit = bh.consume(set.removedAll(absent))
}
