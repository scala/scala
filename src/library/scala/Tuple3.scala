/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala

import scala.collection.{ TraversableLike => TLike, IterableLike => ILike }
import scala.collection.generic.{ CanBuildFrom => CBF }


/** A tuple of 3 elements; the canonical representation of a [[scala.Product3]].
 *
 *  @constructor  Create a new tuple with 3 elements. Note that it is more idiomatic to create a Tuple3 via `(t1, t2, t3)`
 *  @param  _1   Element 1 of this Tuple3
 *  @param  _2   Element 2 of this Tuple3
 *  @param  _3   Element 3 of this Tuple3
 */
case class Tuple3[+T1, +T2, +T3](_1: T1, _2: T2, _3: T3)
  extends Product3[T1, T2, T3]
{
  override def toString() = "(" + _1 + "," + _2 + "," + _3 + ")"


  @deprecated("Use `zipped` instead.", "2.9.0")
  def zip[Repr1, El1, El2, El3, To](implicit w1:   T1 => TLike[El1, Repr1],
                                             w2:   T2 => Iterable[El2],
                                             w3:   T3 => Iterable[El3],
                                             cbf1: CBF[Repr1, (El1, El2, El3), To]): To = {
    zipped map ((x, y, z) => ((x, y, z)))
  }

  /** Wraps a tuple in a `Zipped`, which supports 3-ary generalisations of `map`, `flatMap`, `filter`, etc.
   * Note that there must be an implicit value to convert this tuple's types into a [[scala.collection.TraversableLike]]
   * or [[scala.collection.IterableLike]].
   * {{{
   * scala> val tuple = (List(1,2,3),List('a','b','c'),List("x","y","z"))
   * tuple: (List[Int], List[Char], List[java.lang.String]) = (List(1, 2, 3),List(a, b, c),List(x, y, z))
   *
   * scala> tuple.zipped map { (x,y,z) => x + ":" + y + ":" + z}
   * res8: List[java.lang.String] = List(1:a:x, 2:b:y, 3:c:z)
   * }}}
   *
   * @see Zipped
   * Note: will not terminate for infinite-sized collections.
   */
  def zipped[Repr1, El1, Repr2, El2, Repr3, El3](implicit w1: T1 => TLike[El1, Repr1],
                                                          w2: T2 => ILike[El2, Repr2],
                                                          w3: T3 => ILike[El3, Repr3]): Zipped[Repr1, El1, Repr2, El2, Repr3, El3]
    = new Zipped[Repr1, El1, Repr2, El2, Repr3, El3](_1, _2, _3)

  class Zipped[+Repr1, +El1, +Repr2, +El2, +Repr3, +El3](coll1: TLike[El1, Repr1],
                                                         coll2: ILike[El2, Repr2],
                                                         coll3: ILike[El3, Repr3]) {
    def map[B, To](f: (El1, El2, El3) => B)(implicit cbf: CBF[Repr1, B, To]): To = {
      val b = cbf(coll1.repr)
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext)
          b += f(el1, elems2.next, elems3.next)
        else
          return b.result
      }
      b.result
    }

    def flatMap[B, To](f: (El1, El2, El3) => TraversableOnce[B])(implicit cbf: CBF[Repr1, B, To]): To = {
      val b = cbf(coll1.repr)
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext)
          b ++= f(el1, elems2.next, elems3.next)
        else
          return b.result
      }
      b.result
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
      def result = (b1.result, b2.result, b3.result)

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext) {
          val el2 = elems2.next
          val el3 = elems3.next

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

    def exists(f: (El1, El2, El3) => Boolean): Boolean = {
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext) {
          if (f(el1, elems2.next, elems3.next))
            return true
        }
        else return false
      }
      false
    }

    def forall(f: (El1, El2, El3) => Boolean): Boolean =
      !exists((x, y, z) => !f(x, y, z))

    def foreach[U](f: (El1, El2, El3) => U): Unit = {
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext)
          f(el1, elems2.next, elems3.next)
        else
          return
      }
    }
  }

}
