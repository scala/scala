package scala.collection.parallel
package immutable



import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Arbitrary._

import scala.collection._
import scala.collection.parallel.ops._


abstract class ParallelHashMapCheck[K, V](tp: String) extends ParallelIterableCheck[(K, V)]("immutable.ParHashMap[" + tp + "]") {
  ForkJoinTasks.defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors * 2)
  ForkJoinTasks.defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors * 2)

  type CollType = ParHashMap[K, V]

  def isCheckingViews = false

  def instances(vals: Seq[Gen[(K, V)]]): Gen[Iterable[(K, V)]] = sized { sz =>
    var hm = new immutable.HashMap[K, V]
    val gen = vals(rnd.nextInt(vals.size))
    for (i <- 0 until sz) hm += sample(gen)
    hm
  }

  def fromTraversable(t: Traversable[(K, V)]) = {
    var phm = new ParHashMap[K, V]
    var i = 0
    for (kv <- t.toList) {
      phm += kv
      i += 1
    }
    phm
  }

}


object IntIntParallelHashMapCheck extends ParallelHashMapCheck[Int, Int]("Int, Int")
with PairOperators[Int, Int]
with PairValues[Int, Int]
{
  def intvalues = new IntValues {}
  def kvalues = intvalues.values
  def vvalues = intvalues.values

  val intoperators = new IntOperators {}
  def voperators = intoperators
  def koperators = intoperators
}










