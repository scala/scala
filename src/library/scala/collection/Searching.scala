/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import scala.language.implicitConversions
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
  *    // == Found(2)
  * }}}
  */
object Searching {
  sealed abstract class SearchResult {
    def insertionPoint: Int
  }

  case class Found(foundIndex: Int) extends SearchResult {
    override def insertionPoint = foundIndex
  }
  case class InsertionPoint(insertionPoint: Int) extends SearchResult

  class SearchImpl[A, Repr](private val coll: SeqLike[A, Repr]) extends AnyVal {
    /** Search the sorted sequence for a specific element. If the sequence is an
      * `IndexedSeqLike`, a binary search is used. Otherwise, a linear search is used.
      *
      * The sequence should be sorted with the same `Ordering` before calling; otherwise,
      * the results are undefined.
      *
      * @see [[scala.collection.IndexedSeqLike]]
      * @see [[scala.math.Ordering]]
      * @see [[scala.collection.SeqLike]], method `sorted`
      *
      * @param elem the element to find.
      * @param ord  the ordering to be used to compare elements.
      *
      * @return a `Found` value containing the index corresponding to the element in the
      *         sequence, or the `InsertionPoint` where the element would be inserted if
      *         the element is not in the sequence.
      */
    final def search[B >: A](elem: B)(implicit ord: Ordering[B]): SearchResult =
      coll match {
        case _: IndexedSeqLike[A, Repr] => binarySearchBy(elem, 0, coll.length, identity)(ord)
        case _ => linearSearchBy(coll.view, elem, 0, identity)(ord)
      }

    /** Search within an interval in the sorted sequence for a specific element. If the
      * sequence is an `IndexedSeqLike`, a binary search is used. Otherwise, a linear search
      * is used.
      *
      * The sequence should be sorted with the same `Ordering` before calling; otherwise,
      * the results are undefined.
      *
      * @see [[scala.collection.IndexedSeqLike]]
      * @see [[scala.math.Ordering]]
      * @see [[scala.collection.SeqLike]], method `sorted`
      *
      * @param elem the element to find.
      * @param from the index where the search starts.
      * @param to   the index following where the search ends.
      * @param ord  the ordering to be used to compare elements.
      *
      * @return a `Found` value containing the index corresponding to the element in the
      *         sequence, or the `InsertionPoint` where the element would be inserted if
      *         the element is not in the sequence.
      */
    final def search[B >: A](elem: B, from: Int, to: Int)
    (implicit ord: Ordering[B]): SearchResult =
      coll match {
        case _: IndexedSeqLike[A, Repr] => binarySearchBy(elem, from, to, identity)(ord)
        case _ => linearSearchBy(coll.view(from, to), elem, from, identity)(ord)
      }


    /** Search the sorted sequence for a specific element. If the sequence is an
      * `IndexedSeqLike`, a binary search is used. Otherwise, a linear search is used.
      *
      * The sequence should be sorted with the same `Ordering` before calling; otherwise,
      * the results are undefined.
      *
      * @see [[scala.collection.IndexedSeqLike]]
      * @see [[scala.math.Ordering]]
      * @see [[scala.collection.SeqLike]], method `sorted`
      *
      * @param elem the element to find.
      * @param from the index where the search starts.
      * @param to   the index following where the search ends.*
      * @param by   function to convert elements of collection to ordering elements
      * @param ord  the ordering to be used to compare elements.
      *
      * @return a `Found` value containing the index corresponding to the element in the
      *         sequence, or the `InsertionPoint` where the element would be inserted if
      *         the element is not in the sequence.
      */
    final def searchBy[B](elem:B, from: Int, to: Int, by:A => B)(implicit ord: Ordering[B]):SearchResult = {
      coll match {
        case _: IndexedSeqLike[A, Repr] => binarySearchBy(elem, from, to, by)(ord)
        case _ => linearSearchBy(coll.view(from, to), elem, from, by)(ord)
      }
    }

    /** Search the sorted sequence for a specific element. If the sequence is an
      * `IndexedSeqLike`, a binary search is used. Otherwise, a linear search is used.
      *
      * The sequence should be sorted with the same `Ordering` before calling; otherwise,
      * the results are undefined.
      *
      * @see [[scala.collection.IndexedSeqLike]]
      * @see [[scala.math.Ordering]]
      * @see [[scala.collection.SeqLike]], method `sorted`
      *
      * @param elem the element to find.
      * @param by   function to convert elements of collection to ordering elements
      * @param ord  the ordering to be used to compare elements.
      *
      * @return a `Found` value containing the index corresponding to the element in the
      *         sequence, or the `InsertionPoint` where the element would be inserted if
      *         the element is not in the sequence.
      */
    final def searchBy[B](elem:B, by:A => B)(implicit ord: Ordering[B]):SearchResult = {
      coll match {
        case _: IndexedSeqLike[A, Repr] => binarySearchBy(elem, 0, coll.length, by)(ord)
        case _ => linearSearchBy(coll.view, elem, 0, by)(ord)
      }
    }

    @tailrec
    private def binarySearchBy[B](elem: B, from: Int, to: Int, by:A => B)
    (implicit ord: Ordering[B]): SearchResult = {
      if (to == from) InsertionPoint(from) else {
        val idx = from+(to-from-1)/2
        math.signum(ord.compare(elem, by(coll(idx)))) match {
          case -1 => binarySearchBy(elem, from, idx, by)(ord)
          case  1 => binarySearchBy(elem, idx + 1, to, by)(ord)
          case  _ => Found(idx)
        }
      }
    }

    private def linearSearchBy[B](c: SeqView[A, Repr], elem: B, offset: Int, by: A => B)
    (implicit ord: Ordering[B]): SearchResult = {
      var idx = offset
      val it = c.iterator
      while (it.hasNext) {
        val cur = by(it.next())
        if (ord.equiv(elem, cur)) return Found(idx)
        else if (ord.lt(elem, cur)) return InsertionPoint(idx)
        idx += 1
      }
      InsertionPoint(idx)
    }

  }

  implicit def search[Repr, A](coll: Repr)
  (implicit fr: IsSeqLike[Repr]): SearchImpl[fr.A, Repr] = new SearchImpl(fr.conversion(coll))
}
