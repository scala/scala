/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import scala.collection.{ TraversableLike, IterableLike }
import scala.collection.generic.{ CanBuildFrom => CBF }
import language.{ higherKinds, implicitConversions }

/** This interface is intended as a minimal interface, not complicated
 *  by the requirement to resolve type constructors, for implicit search (which only
 *  needs to find an implicit conversion to Traversable for our purposes.)
 */
trait ZippedTraversable2[+El1, +El2] {
  def foreach[U](f: (El1, El2) => U): Unit
}
object ZippedTraversable2 {
  implicit def zippedTraversable2ToTraversable[El1, El2](zz: ZippedTraversable2[El1, El2]): Traversable[(El1, El2)] = {
    new collection.AbstractTraversable[(El1, El2)] {
      def foreach[U](f: ((El1, El2)) => U): Unit = zz foreach Function.untupled(f)
    }
  }
}

class Tuple2Zipped[El1, Repr1, El2, Repr2](
  coll1: TraversableLike[El1, Repr1],
  coll2: IterableLike[El2, Repr2]
) extends ZippedTraversable2[El1, El2] {
  def map[B, To](f: (El1, El2) => B)(implicit cbf: CBF[Repr1, B, To]): To = {
    val b = cbf(coll1.repr)
    b.sizeHint(coll1)
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext)
        b += f(el1, elems2.next)
      else
        return b.result
    }

    b.result
  }

  def flatMap[B, To](f: (El1, El2) => TraversableOnce[B])(implicit cbf: CBF[Repr1, B, To]): To = {
    val b = cbf(coll1.repr)
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext)
        b ++= f(el1, elems2.next)
      else
        return b.result
    }

    b.result
  }

  def filter[To1, To2](f: (El1, El2) => Boolean)(implicit cbf1: CBF[Repr1, El1, To1], cbf2: CBF[Repr2, El2, To2]): (To1, To2) = {
    val b1 = cbf1(coll1.repr)
    val b2 = cbf2(coll2.repr)
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext) {
        val el2 = elems2.next
        if (f(el1, el2)) {
          b1 += el1
          b2 += el2
        }
      }
      else return (b1.result, b2.result)
    }

    (b1.result, b2.result)
  }

  def exists(f: (El1, El2) => Boolean): Boolean = {
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext) {
        if (f(el1, elems2.next))
          return true
      }
      else return false
    }
    false
  }

  def forall(f: (El1, El2) => Boolean): Boolean =
    !exists((x, y) => !f(x, y))

  def foreach[U](f: (El1, El2) => U): Unit = {
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext)
        f(el1, elems2.next)
      else
        return
    }
  }
}

object Tuple2Zipped {
  class Ops[T1, T2](x: (T1, T2)) {
    def invert[El1, CC1[X] <: TraversableOnce[X], El2, CC2[X] <: TraversableOnce[X], That]
      (implicit w1: T1 <:< CC1[El1],
                w2: T2 <:< CC2[El2],
                bf: collection.generic.CanBuildFrom[CC1[_], (El1, El2), That]
      ): That = {
        val buf = bf(x._1)
        val it1 = x._1.toIterator
        val it2 = x._2.toIterator
        while (it1.hasNext && it2.hasNext)
          buf += ((it1.next, it2.next))
        
        buf.result
      }

    def zipped[El1, Repr1, El2, Repr2]
      (implicit w1: T1 => TraversableLike[El1, Repr1],
                w2: T2 => IterableLike[El2, Repr2]
      ): Tuple2Zipped[El1, Repr1, El2, Repr2] = new Tuple2Zipped(x._1, x._2)
  }
}
