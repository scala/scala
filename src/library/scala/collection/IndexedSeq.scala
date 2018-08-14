package scala
package collection

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.math.Ordering
import Searching.{SearchResult, Found, InsertionPoint}

/** Base trait for indexed sequences that have efficient `apply` and `length` */
trait IndexedSeq[+A] extends Seq[A] with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]] {
  override protected[this] def stringPrefix: String = "IndexedSeq"
}

@SerialVersionUID(3L)
object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](immutable.IndexedSeq)

/** Base trait for indexed Seq operations */
trait IndexedSeqOps[+A, +CC[_], +C] extends Any with SeqOps[A, CC, C] { self =>

  def iterator: Iterator[A] = view.iterator

  override def reverseIterator: Iterator[A] = new AbstractIterator[A] {
    private[this] var i = self.length
    def hasNext: Boolean = 0 < i
    def next(): A =
      if (0 < i) {
        i -= 1
        self(i)
      } else Iterator.empty.next()
  }

  override def view: IndexedSeqView[A] = new IndexedSeqView.Id[A](this)

  override protected def reversed: Iterable[A] = new IndexedSeqView.Reverse(this)

  // Override transformation operations to use more efficient views than the default ones
  override def prepended[B >: A](elem: B): CC[B] = iterableFactory.from(new IndexedSeqView.Prepended(elem, this))

  override def take(n: Int): C = fromSpecific(new IndexedSeqView.Take(this, n))

  override def takeRight(n: Int): C = fromSpecific(new IndexedSeqView.TakeRight(this, n))

  override def drop(n: Int): C = fromSpecific(new IndexedSeqView.Drop(this, n))

  override def dropRight(n: Int): C = fromSpecific(new IndexedSeqView.DropRight(this, n))

  override def map[B](f: A => B): CC[B] = iterableFactory.from(new IndexedSeqView.Map(this, f))

  override def reverse: C = fromSpecific(new IndexedSeqView.Reverse(this))

  override def slice(from: Int, until: Int): C = fromSpecific(new IndexedSeqView.Slice(this, from, until))

  override def last: A = apply(length - 1)

  override final def lengthCompare(len: Int): Int = Integer.compare(length, len)

  final override def knownSize: Int = length

  override final def sizeCompare(that: Iterable[_]): Int = {
    val res = that.sizeCompare(length)
    // can't just invert the result, because `-Int.MinValue == Int.MinValue`
    if (res == Int.MinValue) 1 else -res
  }

  override def search[B >: A](elem: B)(implicit ord: Ordering[B]): SearchResult =
    binarySearch(elem, 0, length)(ord)

  override def search[B >: A](elem: B, from: Int, to: Int)(implicit ord: Ordering[B]): SearchResult =
    binarySearch(elem, from, to)(ord)

  @tailrec
  private[this] def binarySearch[B >: A](elem: B, from: Int, to: Int)
                                        (implicit ord: Ordering[B]): SearchResult = {
    if (from < 0) binarySearch(elem, 0, to)
    else if (to > length) binarySearch(elem, from, length)
    else if (to <= from) InsertionPoint(from)
    else {
      val idx = from + (to - from - 1) / 2
      math.signum(ord.compare(elem, apply(idx))) match {
        case -1 => binarySearch(elem, from, idx)(ord)
        case  1 => binarySearch(elem, idx + 1, to)(ord)
        case  _ => Found(idx)
      }
    }
  }
}
