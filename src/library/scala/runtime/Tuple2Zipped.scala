/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import scala.collection.{ TraversableLike => TLike, IterableLike => ILike }
import scala.collection.generic.{ CanBuildFrom => CBF }

 /** Wraps a tuple in a `Zipped`, which supports 2-ary generalisations of `map`, `flatMap`, `filter`, etc.
  * Note that there must be an implicit value to convert this tuple's types into a [[scala.collection.TraversableLike]]
  * or [[scala.collection.IterableLike]].
  * {{{
  * scala> val tuple = (List(1,2,3),List('a','b','c'))
  * tuple: (List[Int], List[Char]) = (List(1, 2, 3),List(a, b, c))
  *
  * scala> tuple.zipped map { (x,y) => x + ":" + y }
  * res6: List[java.lang.String] = List(1:a, 2:b, 3:c)
  * }}}
  *
  * @see Zipped
  * Note: will not terminate for infinite-sized collections.
  */
@inline private[scala] final class Tuple2Zipped[+Repr1, +El1, +Repr2, +El2](coll1: TLike[El1, Repr1], coll2: ILike[El2, Repr2]) {
  // coll2: ILike for filter
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

@inline private[scala] final class Tuple2ZippedOps[T1, T2](zz: (T1, T2)) {
  def zipped[Repr1, El1, Repr2, El2](implicit w1: T1 => TLike[El1, Repr1], w2: T2 => ILike[El2, Repr2]) =
    new Tuple2Zipped[Repr1, El1, Repr2, El2](w1(zz._1), w2(zz._2))
}
