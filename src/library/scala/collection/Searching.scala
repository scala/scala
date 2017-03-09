/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import scala.annotation.tailrec
import scala.collection.generic.IsSeqLike
import scala.math.Ordering

/** A collection of wrappers that provide sequence classes with search functionality.
  *
  * Example usage:
  * {{{
  *    import scala.collection.Searching._
  *    val l = List(1, 2, 3, 4, 5)
  *    l.search(3)
  *    // == 2
  * }}}
  */
object Searching {
  class SearchImpl[A, Repr](val coll: SeqLike[A, Repr]) {
    /** Search the sorted sequence for a specific element.
      *
      * The sequence should be sorted with the same `Ordering` before calling; otherwise,
      * the results are undefined.
      *
      * @see [[scala.math.Ordering]]
      * @see [[scala.collection.SeqLike]], method `sorted`
      *
      * @param elem the element to find.
      * @param ord  the ordering to be used to compare elements.
      * @return a `Right` value containing the index corresponding to the element in the
      *         $coll, or a `Left` value containing the index where the element would be
      *         inserted if the element is not in the $coll.
      */
    final def search[B >: A](elem: B)(implicit ord: Ordering[B]): Either[Int, Int] = 
      coll match {
        case _: IndexedSeqLike[A, Repr] => binarySearch(elem, -1, coll.length)(ord)
        case _ => linearSearch(coll.view, elem, 0)(ord)
      }

    /** Search within an interval in the sorted sequence for a specific element.
      *
      * The sequence should be sorted with the same `Ordering` before calling; otherwise,
      * the results are undefined.
      *
      * @see [[scala.math.Ordering]]
      * @see [[scala.collection.SeqLike]], method `sorted`
      *
      * @param elem the element to find.
      * @param from the index where the search starts.
      * @param to   the index following where the search ends.
      * @param ord  the ordering to be used to compare elements.
      * @return a `Right` value containing the index corresponding to the element in the
      *         $coll, or a `Left` value containing the index where the element would be
      *         inserted if the element is not in the $coll.
      */
    final def search[B >: A](elem: B, from: Int, to: Int)
    (implicit ord: Ordering[B]): Either[Int, Int] = 
      coll match {
        case _: IndexedSeqLike[A, Repr] => binarySearch(elem, from-1, to)(ord)
        case _ => linearSearch(coll.view(from, to), elem, from)(ord)
      }

    @tailrec
    private def binarySearch[B >: A](elem: B, from: Int, to: Int)
    (implicit ord: Ordering[B]): Either[Int, Int] = {
      if ((to-from) == 1) Left(from) else {
        val idx = (to+from)/2
        math.signum(ord.compare(elem, coll(idx))) match {
          case -1 => binarySearch(elem, from, idx)(ord)
          case  1 => binarySearch(elem, idx, to)(ord)
          case  _ => Right(idx)
        }
      }
    }

    private def linearSearch[B >: A](c: SeqView[A, Repr], elem: B, offset: Int)
    (implicit ord: Ordering[B]): Either[Int, Int] = {
      var idx = offset
      val it = c.iterator
      while (it.hasNext) {
        val cur = it.next()
        if (ord.equiv(elem, cur)) return Right(idx)
        else if (ord.lt(elem, cur)) return Left(idx-1)
        idx += 1
      }
      Left(idx)
    }

  }

  implicit def search[Repr, A](coll: Repr)
  (implicit fr: IsSeqLike[Repr]): SearchImpl[fr.A, Repr] = new SearchImpl(fr.conversion(coll))
}
