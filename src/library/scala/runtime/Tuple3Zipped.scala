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

/** See comment on ZippedTraversable2
 *  @define Coll `ZippedTraversable3`
 *  @define coll collection
 *  @define collectExample
 *  @define willNotTerminateInf
 */
trait ZippedTraversable3[+El1, +El2, +El3] extends Any {
  def foreach[U](f: (El1, El2, El3) => U): Unit
}
object ZippedTraversable3 {
  implicit def zippedTraversable3ToTraversable[El1, El2, El3](zz: ZippedTraversable3[El1, El2, El3]): Traversable[(El1, El2, El3)] = {
    new scala.collection.AbstractTraversable[(El1, El2, El3)] {
      def foreach[U](f: ((El1, El2, El3)) => U): Unit = zz foreach Function.untupled(f)
    }
  }
}

final class Tuple3Zipped[El1, Repr1, El2, Repr2, El3, Repr3](val colls: (TraversableLike[El1, Repr1], IterableLike[El2, Repr2], IterableLike[El3, Repr3]))
        extends AnyVal with ZippedTraversable3[El1, El2, El3] {

  private def coll1 = colls._1
  private def coll2 = colls._2
  private def coll3 = colls._3

  def map[B, To](f: (El1, El2, El3) => B)(implicit cbf: CBF[Repr1, B, To]): To = {
    val b = cbf(coll1.repr)
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext && elems3.hasNext)
        b += f(el1, elems2.next(), elems3.next())
      else
        return b.result()
    }
    b.result()
  }

  def flatMap[B, To](f: (El1, El2, El3) => TraversableOnce[B])(implicit cbf: CBF[Repr1, B, To]): To = {
    val b = cbf(coll1.repr)
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext && elems3.hasNext)
        b ++= f(el1, elems2.next(), elems3.next())
      else
        return b.result()
    }
    b.result()
  }

  def filter[To1, To2, To3](f: (El1, El2, El3) => Boolean)(
               implicit cbf1: CBF[Repr1, El1, To1],
                        cbf2: CBF[Repr2, El2, To2],
                        cbf3: CBF[Repr3, El3, To3]): (To1, To2, To3) = {
    val b1 = cbf1(coll1.repr)
    val b2 = cbf2(coll2.repr)
    val b3 = cbf3(coll3.repr)
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator
    def result = (b1.result(), b2.result(), b3.result())

    for (el1 <- coll1) {
      if (elems2.hasNext && elems3.hasNext) {
        val el2 = elems2.next()
        val el3 = elems3.next()

        if (f(el1, el2, el3)) {
          b1 += el1
          b2 += el2
          b3 += el3
        }
      }
      else return result
    }

    result
  }

  def exists(@deprecatedName('f) p: (El1, El2, El3) => Boolean): Boolean = {
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext && elems3.hasNext) {
        if (p(el1, elems2.next(), elems3.next()))
          return true
      }
      else return false
    }
    false
  }

  def forall(@deprecatedName('f) p: (El1, El2, El3) => Boolean): Boolean =
    !exists((x, y, z) => !p(x, y, z))

  def foreach[U](f: (El1, El2, El3) => U): Unit = {
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext && elems3.hasNext)
        f(el1, elems2.next(), elems3.next())
      else
        return
    }
  }

  override def toString = s"($coll1, $coll2, $coll3).zipped"
}

object Tuple3Zipped {
  final class Ops[T1, T2, T3](private val x: (T1, T2, T3)) extends AnyVal {
    def invert[El1, CC1[X] <: TraversableOnce[X], El2, CC2[X] <: TraversableOnce[X], El3, CC3[X] <: TraversableOnce[X], That]
      (implicit w1: T1 <:< CC1[El1],
                w2: T2 <:< CC2[El2],
                w3: T3 <:< CC3[El3],
                bf: scala.collection.generic.CanBuildFrom[CC1[_], (El1, El2, El3), That]
      ): That = {
        val buf = bf(x._1)
        val it1 = x._1.toIterator
        val it2 = x._2.toIterator
        val it3 = x._3.toIterator
        while (it1.hasNext && it2.hasNext && it3.hasNext)
          buf += ((it1.next(), it2.next(), it3.next()))

        buf.result()
      }

    def zipped[El1, Repr1, El2, Repr2, El3, Repr3]
      (implicit w1: T1 => TraversableLike[El1, Repr1],
                w2: T2 => IterableLike[El2, Repr2],
                w3: T3 => IterableLike[El3, Repr3]
      ): Tuple3Zipped[El1, Repr1, El2, Repr2, El3, Repr3] = new Tuple3Zipped((x._1, x._2, x._3))
  }
}
