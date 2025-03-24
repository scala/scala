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

/** See comment on ZippedIterable2
 *  @define Coll `ZippedIterable3`
 *  @define coll collection
 *  @define collectExample
 *  @define willNotTerminateInf
 */
@deprecated("Use scala.collection.LazyZip3.", "2.13.0")
trait ZippedIterable3[+El1, +El2, +El3] extends Any {
  def iterator: Iterator[(El1, El2, El3)]
  def isEmpty: Boolean
}
@deprecated("Use scala.collection.LazyZip3.", "2.13.0")
object ZippedIterable3 {
  implicit def zippedIterable3ToIterable[El1, El2, El3](zz: ZippedIterable3[El1, El2, El3]): Iterable[(El1, El2, El3)] = {
    new scala.collection.AbstractIterable[(El1, El2, El3)] {
      def iterator: Iterator[(El1, El2, El3)] = zz.iterator
      override def isEmpty: Boolean = zz.isEmpty
    }
  }
}

@deprecated("Use scala.collection.LazyZip3.", "2.13.0")
final class Tuple3Zipped[El1, It1 <: Iterable[El1], El2, It2 <: Iterable[El2], El3, It3 <: Iterable[El3]](private val colls: (It1, It2, It3))
        extends AnyVal with ZippedIterable3[El1, El2, El3] {

  private def coll1 = colls._1
  private def coll2 = colls._2
  private def coll3 = colls._3

  def map[B, To](f: (El1, El2, El3) => B)(implicit bf: BuildFrom[It1, B, To]): To = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      b += f(elems1.next(), elems2.next(), elems3.next())
    }
    b.result()
  }

  def flatMap[B, To](f: (El1, El2, El3) => IterableOnce[B])(implicit bf: BuildFrom[It1, B, To]): To = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      b ++= f(elems1.next(), elems2.next(), elems3.next())
    }
    b.result()
  }

  def filter[To1, To2, To3](f: (El1, El2, El3) => Boolean)(
               implicit bf1: BuildFrom[It1, El1, To1],
                        bf2: BuildFrom[It2, El2, To2],
                        bf3: BuildFrom[It3, El3, To3]): (To1, To2, To3) = {
    val b1 = bf1.newBuilder(coll1)
    val b2 = bf2.newBuilder(coll2)
    val b3 = bf3.newBuilder(coll3)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      val el1 = elems1.next()
      val el2 = elems2.next()
      val el3 = elems3.next()

      if (f(el1, el2, el3)) {
        b1 += el1
        b2 += el2
        b3 += el3
      }
    }
    (b1.result(), b2.result(), b3.result())
  }

  def exists(p: (El1, El2, El3) => Boolean): Boolean = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      if (p(elems1.next(), elems2.next(), elems3.next())) {
        return true
      }
    }
    false
  }

  def forall(p: (El1, El2, El3) => Boolean): Boolean =
    !exists((x, y, z) => !p(x, y, z))

  def iterator: Iterator[(El1, El2, El3)] = coll1.iterator.zip(coll2.iterator).zip(coll3.iterator).map { case ((a, b), c) => (a, b, c)}
  override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty || coll3.isEmpty
  def foreach[U](f: (El1, El2, El3) => U): Unit = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      f(elems1.next(), elems2.next(), elems3.next())
    }
  }

  override def toString = s"($coll1, $coll2, $coll3).zipped"
}

@deprecated("Use scala.collection.LazyZip3.", since = "2.13.0")
object Tuple3Zipped {
  final class Ops[T1, T2, T3](private val x: (T1, T2, T3)) extends AnyVal {
    @deprecated("Use xs.lazyZip(yz).lazyZip(zs).map((_, _, _))", since = "2.13.0")
    def invert[El1, It1[a] <: Iterable[a], El2, It2[a] <: Iterable[a], El3, It3[a] <: Iterable[a], That]
      (implicit w1: T1 <:< It1[El1],
                w2: T2 <:< It2[El2],
                w3: T3 <:< It3[El3],
                bf: BuildFrom[T1, (El1, El2, El3), That]
      ): That = {
        val buf = bf.newBuilder(x._1)
        val it1 = x._1.iterator
        val it2 = x._2.iterator
        val it3 = x._3.iterator
        while (it1.hasNext && it2.hasNext && it3.hasNext)
          buf += ((it1.next(), it2.next(), it3.next()))

        buf.result()
      }

    @deprecated("Use xs.lazyZip(ys).lazyZip(zs)", since = "2.13.0")
    def zipped[El1, It1 <: Iterable[El1], El2, It2 <: Iterable[El2], El3, It3 <: Iterable[El3]]
      (implicit w1: T1 => IterableOps[El1, Iterable, It1] with It1,
                w2: T2 => IterableOps[El2, Iterable, It2] with It2,
                w3: T3 => IterableOps[El3, Iterable, It3] with It3
      ): Tuple3Zipped[El1, It1, El2, It2, El3, It3] = new Tuple3Zipped((w1(x._1), w2(x._2), w3(x._3)))
  }
}
