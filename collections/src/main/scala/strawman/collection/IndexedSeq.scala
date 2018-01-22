package strawman
package collection

import scala.{Any, Boolean, Int}
import scala.annotation.tailrec
import scala.math
import scala.math.Ordering
import Searching.{SearchResult, Found, InsertionPoint}

/** Base trait for indexed sequences that have efficient `apply` and `length` */
trait IndexedSeq[+A] extends Seq[A] with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]

object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](immutable.IndexedSeq)

/** Base trait for indexed Seq operations */
trait IndexedSeqOps[+A, +CC[X] <: IndexedSeq[X], +C] extends Any with SeqOps[A, CC, C] with ArrayLike[A] { self =>

  def iterator(): Iterator[A] = view.iterator()

  final override def size: Int = finiteSize

  final override def knownSize: Int = finiteSize

  override def reverseIterator(): Iterator[A] = new AbstractIterator[A] {
    private var i = self.size
    def hasNext: Boolean = 0 < i
    def next(): A =
      if (0 < i) {
        i -= 1
        self(i)
      } else Iterator.empty.next()
  }

  override def view: IndexedView[A] = new IndexedView[A] {
    protected def finiteSize: Int = self.size
    def apply(i: Int): A = self(i)
  }

  override protected[this] def reversed: Iterable[A] = view.reverse

  /** A collection containing the last `n` elements of this collection. */
  override def takeRight(n: Int): C = fromSpecificIterable(view.takeRight(n))

  /** The rest of the collection without its `n` last elements. For
    * linear, immutable collections this should avoid making a copy. */
  override def dropRight(n: Int): C = fromSpecificIterable(view.dropRight(n))

  override def lengthCompare(len: Int): Int = size - len

  override def search[B >: A](elem: B)(implicit ord: Ordering[B]): SearchResult =
    binarySearch(elem, 0, length)(ord)

  override def search[B >: A](elem: B, from: Int, to: Int)(implicit ord: Ordering[B]): SearchResult =
    binarySearch(elem, from, to)(ord)

  @tailrec
  private[this] def binarySearch[B >: A](elem: B, from: Int, to: Int)
                                        (implicit ord: Ordering[B]): SearchResult = {
    if (to == from) InsertionPoint(from) else {
      val idx = from+(to-from-1)/2
      math.signum(ord.compare(elem, apply(idx))) match {
        case -1 => binarySearch(elem, from, idx)(ord)
        case  1 => binarySearch(elem, idx + 1, to)(ord)
        case  _ => Found(idx)
      }
    }
  }
}
