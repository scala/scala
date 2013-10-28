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


abstract class ParallelHashSetCheck[T](tp: String) extends ParallelSetCheck[T]("mutable.ParHashSet[" + tp + "]") {
  // ForkJoinTasks.defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors * 2)
  // ForkJoinTasks.defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors * 2)

  type CollType = ParHashSet[T]

  def isCheckingViews = false

  def hasStrictOrder = false

  def tasksupport: TaskSupport

  def ofSize(vals: Seq[Gen[T]], sz: Int) = {
    val hm = new mutable.HashSet[T]
    val gen = vals(rnd.nextInt(vals.size))
    for (i <- 0 until sz) hm += sample(gen)
    hm
  }

  def fromTraversable(t: Traversable[T]) = {
    val phs = new ParHashSet[T]
    phs.tasksupport = tasksupport
    var i = 0
    for (kv <- t.toList) {
      phs += kv
      i += 1
    }
    phs
  }

}


class IntParallelHashSetCheck(val tasksupport: TaskSupport) extends ParallelHashSetCheck[Int]("Int")
with IntOperators
with IntValues
{
  override def printDataStructureDebugInfo(ds: AnyRef) = ds match {
    case pm: ParHashSet[t] =>
      println("Mutable parallel hash set")
    case _ =>
      println("could not match data structure type: " + ds.getClass)
  }

  override def checkDataStructureInvariants(orig: Traversable[Int], ds: AnyRef) = ds match {
    // case pm: ParHashSet[t] if 1 == 0 =>
    //   // for an example of how not to write code proceed below
    //   val invs = pm.brokenInvariants

    //   val containsall = (for (elem <- orig) yield {
    //     if (pm.asInstanceOf[ParHashSet[Int]](elem) == true) true
    //     else {
    //       println("Does not contain original element: " + elem)
    //       println(pm.hashTableContents.table.find(_ == elem))
    //       println(pm.hashTableContents.table.indexOf(elem))
    //       false
    //     }
    //   }).foldLeft(true)(_ && _)


    //   if (invs.isEmpty) {
    //     if (!containsall) println(pm.debugInformation)
    //     containsall
    //   } else {
    //     println("Invariants broken:\n" + invs.mkString("\n"))
    //     false
    //   }
    case _ => true
  }

}










