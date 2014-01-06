package scala.collection.parallel
package mutable



import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Arbitrary._

import scala.collection._
import scala.collection.parallel.ops._



abstract class ParallelConcurrentTrieMapCheck[K, V](tp: String) extends ParallelMapCheck[K, V]("mutable.ParConcurrentTrieMap[" + tp + "]") {
  // ForkJoinTasks.defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors * 2)
  // ForkJoinTasks.defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors * 2)

  type CollType = ParTrieMap[K, V]

  def isCheckingViews = false

  def hasStrictOrder = false

  def tasksupport: TaskSupport

  def ofSize(vals: Seq[Gen[(K, V)]], sz: Int) = {
    val ct = new concurrent.TrieMap[K, V]
    val gen = vals(rnd.nextInt(vals.size))
    for (i <- 0 until sz) ct += sample(gen)
    ct
  }

  def fromTraversable(t: Traversable[(K, V)]) = {
    val pct = new ParTrieMap[K, V]
    pct.tasksupport = tasksupport
    var i = 0
    for (kv <- t.toList) {
      pct += kv
      i += 1
    }
    pct
  }

}


class IntIntParallelConcurrentTrieMapCheck(val tasksupport: TaskSupport) extends ParallelConcurrentTrieMapCheck[Int, Int]("Int, Int")
with PairOperators[Int, Int]
with PairValues[Int, Int]
{
  def intvalues = new IntValues {}
  def kvalues = intvalues.values
  def vvalues = intvalues.values

  val intoperators = new IntOperators {}
  def voperators = intoperators
  def koperators = intoperators

  override def printDataStructureDebugInfo(ds: AnyRef) = ds match {
    case pm: ParTrieMap[k, v] =>
      println("Mutable parallel ctrie")
    case _ =>
      println("could not match data structure type: " + ds.getClass)
  }

  override def checkDataStructureInvariants(orig: Traversable[(Int, Int)], ds: AnyRef) = ds match {
    // case pm: ParHashMap[k, v] if 1 == 0 => // disabled this to make tests faster
    //   val invs = pm.brokenInvariants

    //   val containsall = (for ((k, v) <- orig) yield {
    //     if (pm.asInstanceOf[ParHashMap[Int, Int]].get(k) == Some(v)) true
    //     else {
    //       println("Does not contain original element: " + (k, v))
    //       false
    //     }
    //   }).foldLeft(true)(_ && _)


    //   if (invs.isEmpty) containsall
    //   else {
    //     println("Invariants broken:\n" + invs.mkString("\n"))
    //     false
    //   }
    case _ => true
  }

}










