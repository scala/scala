/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package runtime


import scala.collection.{BuildFrom, IterableOps}
import scala.language.implicitConversions

/** This interface is intended as a minimal interface, not complicated
 *  by the requirement to resolve type constructors, for implicit search (which only
 *  needs to find an implicit conversion to Iterable for our purposes.)
 *  @define Coll `ZippedIterable2`
 *  @define coll collection
 *  @define collectExample
 *  @define willNotTerminateInf
 */
@deprecated("Use scala.collection.LazyZip2.", "2.13.0")
trait ZippedIterable2[+El1, +El2] extends Any {
  def iterator: Iterator[(El1, El2)]
  def isEmpty: Boolean
}
@deprecated("Use scala.collection.LazyZip2.", "2.13.0")
object ZippedIterable2 {
  implicit def zippedIterable2ToIterable[El1, El2](zz: ZippedIterable2[El1, El2]): Iterable[(El1, El2)] = {
    new scala.collection.AbstractIterable[(El1, El2)] {
      def iterator: Iterator[(El1, El2)] = zz.iterator
      override def isEmpty: Boolean = zz.isEmpty
    }
  }
}

@deprecated("Use scala.collection.LazyZip2.", "2.13.0")
final class Tuple2Zipped[El1, It1 <: Iterable[El1], El2, It2 <: Iterable[El2]](private val colls: (It1, It2)) extends AnyVal with ZippedIterable2[El1, El2] {
  private def coll1 = colls._1
  private def coll2 = colls._2

  def map[B, To](f: (El1, El2) => B)(implicit bf: BuildFrom[It1, B, To]): To = {
    val b = bf.newBuilder(coll1)
    b.sizeHint(coll1, delta = 0)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) {
      b += f(elems1.next(), elems2.next())
    }

    b.result()
  }

  def flatMap[B, To](f: (El1, El2) => IterableOnce[B])(implicit bf: BuildFrom[It1, B, To]): To = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) {
      b ++= f(elems1.next(), elems2.next())
    }

    b.result()
  }

  def filter[To1, To2](f: (El1, El2) => Boolean)(implicit bf1: BuildFrom[It1, El1, To1], bf2: BuildFrom[It2, El2, To2]): (To1, To2) = {
    val b1 = bf1.newBuilder(coll1)
    val b2 = bf2.newBuilder(coll2)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) {
      val el1 = elems1.next()
      val el2 = elems2.next()
      if (f(el1, el2)) {
        b1 += el1
        b2 += el2
      }
    }

    (b1.result(), b2.result())
  }

  def exists(p: (El1, El2) => Boolean): Boolean = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) {
      if (p(elems1.next(), elems2.next())) {
        return true
      }
    }
    false
  }

  def forall(p: (El1, El2) => Boolean): Boolean =
    !exists((x, y) => !p(x, y))

  def iterator: Iterator[(El1, El2)] = coll1.iterator.zip(coll2.iterator)
  override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty
  def foreach[U](f: (El1, El2) => U): Unit = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) {
      f(elems1.next(), elems2.next())
    }
  }

  override def toString = s"($coll1, $coll2).zipped"
}

@deprecated("Use scala.collection.LazyZip2.", since = "2.13.0")
object Tuple2Zipped {
  final class Ops[T1, T2](private val x: (T1, T2)) extends AnyVal {
    @deprecated("Use xs.lazyZip(yz).map((_, _))", since = "2.13.0")
    def invert[El1, It1[a] <: Iterable[a], El2, It2[a] <: Iterable[a], That]
      (implicit w1: T1 <:< It1[El1],
                w2: T2 <:< It2[El2],
                bf: BuildFrom[T1, (El1, El2), That]
      ): That = {
        val buf = bf.newBuilder(x._1)
        val it1 = x._1.iterator
        val it2 = x._2.iterator
        while (it1.hasNext && it2.hasNext)
          buf += ((it1.next(), it2.next()))

        buf.result()
      }

    @deprecated("Use xs.lazyZip(ys)", since = "2.13.0")
    def zipped[El1, It1 <: Iterable[El1], El2, It2 <: Iterable[El2]]
      (implicit w1: T1 => IterableOps[El1, Iterable, It1] with It1,
                w2: T2 => IterableOps[El2, Iterable, It2] with It2
      ): Tuple2Zipped[El1, It1, El2, It2] = new Tuple2Zipped((w1(x._1), w2(x._2)))
  }
}
