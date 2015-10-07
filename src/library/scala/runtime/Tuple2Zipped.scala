/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime


import scala.collection.{ TraversableLike, IterableLike }
import scala.collection.generic.{ CanBuildFrom => CBF }
import scala.language.{ higherKinds, implicitConversions }

/** This interface is intended as a minimal interface, not complicated
 *  by the requirement to resolve type constructors, for implicit search (which only
 *  needs to find an implicit conversion to Traversable for our purposes.)
 *  @define Coll `ZippedTraversable2`
 *  @define coll collection
 *  @define collectExample
 *  @define willNotTerminateInf
 */
trait ZippedTraversable2[+El1, +El2] extends Any {
  def foreach[U](f: (El1, El2) => U): Unit
}
object ZippedTraversable2 {
  implicit def zippedTraversable2ToTraversable[El1, El2](zz: ZippedTraversable2[El1, El2]): Traversable[(El1, El2)] = {
    new scala.collection.AbstractTraversable[(El1, El2)] {
      def foreach[U](f: ((El1, El2)) => U): Unit = zz foreach Function.untupled(f)
    }
  }
}

final class Tuple2Zipped[El1, Repr1, El2, Repr2](val colls: (TraversableLike[El1, Repr1], IterableLike[El2, Repr2])) extends AnyVal with ZippedTraversable2[El1, El2] {
  // This would be better as "private def coll1 = colls._1" but
  // SI-6215 precludes private methods in value classes.
  def map[B, To](f: (El1, El2) => B)(implicit cbf: CBF[Repr1, B, To]): To = {
    val b = cbf(colls._1.repr)
    b.sizeHint(colls._1)
    val elems2 = colls._2.iterator

    for (el1 <- colls._1) {
      if (elems2.hasNext)
        b += f(el1, elems2.next())
      else
        return b.result()
    }

    b.result()
  }

  def flatMap[B, To](f: (El1, El2) => TraversableOnce[B])(implicit cbf: CBF[Repr1, B, To]): To = {
    val b = cbf(colls._1.repr)
    val elems2 = colls._2.iterator

    for (el1 <- colls._1) {
      if (elems2.hasNext)
        b ++= f(el1, elems2.next())
      else
        return b.result()
    }

    b.result()
  }

  def filter[To1, To2](f: (El1, El2) => Boolean)(implicit cbf1: CBF[Repr1, El1, To1], cbf2: CBF[Repr2, El2, To2]): (To1, To2) = {
    val b1 = cbf1(colls._1.repr)
    val b2 = cbf2(colls._2.repr)
    val elems2 = colls._2.iterator

    for (el1 <- colls._1) {
      if (elems2.hasNext) {
        val el2 = elems2.next()
        if (f(el1, el2)) {
          b1 += el1
          b2 += el2
        }
      }
      else return (b1.result(), b2.result())
    }

    (b1.result(), b2.result())
  }

  def exists(@deprecatedName('f) p: (El1, El2) => Boolean): Boolean = {
    val elems2 = colls._2.iterator

    for (el1 <- colls._1) {
      if (elems2.hasNext) {
        if (p(el1, elems2.next()))
          return true
      }
      else return false
    }
    false
  }

  def forall(@deprecatedName('f) p: (El1, El2) => Boolean): Boolean =
    !exists((x, y) => !p(x, y))

  def foreach[U](f: (El1, El2) => U): Unit = {
    val elems2 = colls._2.iterator

    for (el1 <- colls._1) {
      if (elems2.hasNext)
        f(el1, elems2.next())
      else
        return
    }
  }
}

object Tuple2Zipped {
  final class Ops[T1, T2](val x: (T1, T2)) extends AnyVal {
    def invert[El1, CC1[X] <: TraversableOnce[X], El2, CC2[X] <: TraversableOnce[X], That]
      (implicit w1: T1 <:< CC1[El1],
                w2: T2 <:< CC2[El2],
                bf: scala.collection.generic.CanBuildFrom[CC1[_], (El1, El2), That]
      ): That = {
        val buf = bf(x._1)
        val it1 = x._1.toIterator
        val it2 = x._2.toIterator
        while (it1.hasNext && it2.hasNext)
          buf += ((it1.next(), it2.next()))

        buf.result()
      }

    def zipped[El1, Repr1, El2, Repr2]
      (implicit w1: T1 => TraversableLike[El1, Repr1],
                w2: T2 => IterableLike[El2, Repr2]
      ): Tuple2Zipped[El1, Repr1, El2, Repr2] = new Tuple2Zipped((x._1, x._2))
  }
}
