/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package runtime


import scala.collection.{ TraversableLike, IterableLike }
import scala.language.{ higherKinds, implicitConversions }
import scala.{ Iterable, IterableOnce }

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
  implicit def zippedTraversable2ToTraversable[El1, El2](zz: ZippedTraversable2[El1, El2]): Iterable[(El1, El2)] = {
    new scala.collection.AbstractTraversable[(El1, El2)] {
      def foreach[U](f: ((El1, El2)) => U): Unit = zz foreach Function.untupled(f)
    }
  }
}

final class Tuple2Zipped[El1, Repr1, El2, Repr2](val colls: (TraversableLike[El1, Repr1], IterableLike[El2, Repr2])) extends AnyVal with ZippedTraversable2[El1, El2] {
  private def coll1 = colls._1
  private def coll2 = colls._2

  def map[B, To](f: (El1, El2) => B)(implicit cbf: BuildFrom[Repr1, B, To]): To = {
    val b = cbf.newBuilder(coll1.repr)
    b.sizeHint(coll1)
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext)
        b += f(el1, elems2.next())
      else
        return b.result()
    }

    b.result()
  }

  def flatMap[B, To](f: (El1, El2) => IterableOnceIterableOnce[B])(implicit cbf: BuildFrom[Repr1, B, To]): To = {
    val b = cbf.newBuilder(coll1.repr)
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext)
        b ++= f(el1, elems2.next())
      else
        return b.result()
    }

    b.result()
  }

  def filter[To1, To2](f: (El1, El2) => Boolean)(implicit cbf1: BuildFrom[Repr1, El1, To1], cbf2: BuildFrom[Repr2, El2, To2]): (To1, To2) = {
    val b1 = cbf1.newBuilder(coll1.repr)
    val b2 = cbf2.newBuilder(coll2.repr)
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
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
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
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
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext)
        f(el1, elems2.next())
      else
        return
    }
  }

  override def toString = s"($coll1, $coll2).zipped"
}

object Tuple2Zipped {
  final class Ops[T1, T2](private val x: (T1, T2)) extends AnyVal {
    def invert[El1, CC1[X] <: IterableOnceIterableOnce[X], El2, CC2[X] <: IterableOnceIterableOnce[X], That]
      (implicit w1: T1 <:< CC1[El1],
                w2: T2 <:< CC2[El2],
                bf: BuildFrom[CC1[_], (El1, El2), That]
      ): That = {
        val buf = bf.newBuilder(x._1)
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
