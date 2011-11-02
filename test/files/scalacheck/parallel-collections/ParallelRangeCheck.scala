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




object ParallelRangeCheck extends ParallelSeqCheck[Int]("ParallelRange[Int]") with ops.IntSeqOperators {
  // ForkJoinTasks.defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors * 2)
  // ForkJoinTasks.defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors * 2)
  
  type CollType = collection.parallel.ParSeq[Int]
  
  def hasStrictOrder = true
  
  def isCheckingViews = false
  
  def ofSize(vals: Seq[Gen[Int]], sz: Int) = unsupported
  
  override def instances(vals: Seq[Gen[Int]]): Gen[Seq[Int]] = sized { start =>
    sized { end =>
      sized { step =>
        new Range(start, end, if (step != 0) step else 1)
      }
    }
  }
  
  def fromSeq(a: Seq[Int]) = a match {
    case r: Range => ParRange(r.start, r.end, r.step, false)
    case _ =>
      val pa = new parallel.mutable.ParArray[Int](a.length)
      for (i <- 0 until a.length) pa(i) = a(i)
      pa
  }
  
  override def traversable2Seq(t: Traversable[Int]): Seq[Int] = t match {
    case r: Range => r
    case _ => t.toSeq
  }
  
  def values = Seq(choose(-100, 100))
  
}
















