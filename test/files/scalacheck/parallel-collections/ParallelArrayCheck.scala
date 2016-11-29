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


abstract class ParallelArrayCheck[T](tp: String) extends ParallelSeqCheck[T]("ParArray[" + tp + "]") {
  // ForkJoinTasks.defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors * 2)
  // ForkJoinTasks.defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors * 2)

  type CollType = ParArray[T]

  def isCheckingViews = false

  def hasStrictOrder = true

  def tasksupport: TaskSupport

  def ofSize(vals: Seq[Gen[T]], sz: Int) = {
    val a = new mutable.ArrayBuffer[T](sz)
    val gen = vals(rnd.nextInt(vals.size))
    for (i <- 0 until sz) a += sample(gen)
    a
  }

  def fromSeq(a: Seq[T]) = {
    val pa = new ParArray[T](a.size)
    pa.tasksupport = tasksupport
    var i = 0
    for (elem <- a.toList) {
      pa(i) = elem
      i += 1
    }
    pa
  }

  property("array mappings must be equal") = forAllNoShrink(collectionPairs) { case (t, coll) =>
    val results = for ((f, ind) <- mapFunctions.zipWithIndex)
      yield ("op index: " + ind) |: t.map(f) == coll.map(f)
    results.reduceLeft(_ && _)
  }

}


class IntParallelArrayCheck(val tasksupport: TaskSupport) extends ParallelArrayCheck[Int]("Int") with IntSeqOperators with IntValues {
  override def instances(vals: Seq[Gen[Int]]) = oneOf(super.instances(vals), sized { sz =>
    (0 until sz).toArray.toSeq
  }, sized { sz =>
    (-sz until 0).toArray.toSeq
  })
}










