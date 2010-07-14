package scala.collection


import java.lang.Thread._

import scala.collection.generic.CanBuildFrom
import scala.collection.generic.CanCombineFrom


/** Package object for parallel collections.
 */
package object parallel {
  val MIN_FOR_COPY = -1 // TODO: set to 5000
  val CHECK_RATE = 512

  /** Computes threshold from the size of the collection and the parallelism level.
   */
  def thresholdFromSize(sz: Int, parallelismLevel: Int) = {
    val p = parallelismLevel
    if (p > 1) 1 + sz / (8 * p)
    else sz
  }

  /** An implicit conversion providing arrays with a `par` method, which
   *  returns a parallel array.
   *
   *  @tparam T      type of the elements in the array, which is a subtype of AnyRef
   *  @param array   the array to be parallelized
   *  @return        a `Parallelizable` object with a `par` method
   */
  implicit def array2ParallelArray[T <: AnyRef](array: Array[T]) = new Parallelizable[mutable.ParallelArray[T]] {
    def par = mutable.ParallelArray.handoff[T](array)
  }

  implicit def factory2ops[From, Elem, To](bf: CanBuildFrom[From, Elem, To]) = new {
    def isParallel = bf.isInstanceOf[Parallel]
    def asParallel = bf.asInstanceOf[CanCombineFrom[From, Elem, To]]
    def ifParallel[R](isbody: CanCombineFrom[From, Elem, To] => R) = new {
      def otherwise(notbody: => R) = if (isParallel) isbody(asParallel) else notbody
    }
  }

  implicit def traversable2ops[T](t: TraversableOnce[T]) = new {
    def isParallel = t.isInstanceOf[Parallel]
    def isParallelIterable = t.isInstanceOf[ParallelIterable[_]]
    def asParallelIterable = t.asInstanceOf[ParallelIterable[T]]
    def isParallelSeq = t.isInstanceOf[ParallelSeq[_]]
    def asParallelSeq = t.asInstanceOf[ParallelSeq[T]]
    def ifParallelSeq[R](isbody: ParallelSeq[T] => R) = new {
      def otherwise(notbody: => R) = if (isParallel) isbody(asParallelSeq) else notbody
    }
  }

}
















