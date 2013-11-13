package scala.collection
package parallel.immutable



import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Arbitrary._

import scala.collection._
import scala.collection.parallel.ops._


import immutable.Vector
import immutable.VectorBuilder

import scala.collection.parallel.TaskSupport




abstract class ParallelVectorCheck[T](tp: String) extends collection.parallel.ParallelSeqCheck[T]("ParVector[" + tp + "]") {
  // ForkJoinTasks.defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors * 2)
  // ForkJoinTasks.defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors * 2)

  type CollType = ParVector[T]

  def isCheckingViews = false

  def hasStrictOrder = true

  def tasksupport: TaskSupport

  def ofSize(vals: Seq[Gen[T]], sz: Int) = {
    val vb = new immutable.VectorBuilder[T]()
    val gen = vals(rnd.nextInt(vals.size))
    for (i <- 0 until sz) vb += sample(gen)
    vb.result
  }

  def fromSeq(a: Seq[T]) = {
    val pc = ParVector.newCombiner[T]
    for (elem <- a.toList) pc += elem
    val pv = pc.result
    pv.tasksupport = tasksupport
    pv
  }

}



class IntParallelVectorCheck(val tasksupport: TaskSupport) extends ParallelVectorCheck[Int]("Int") with IntSeqOperators with IntValues {
  override def instances(vals: Seq[Gen[Int]]) = oneOf(super.instances(vals), sized { sz =>
    (0 until sz).toArray.toSeq
  }, sized { sz =>
    (-sz until 0).toArray.toSeq
  })
}





