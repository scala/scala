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
package collection

import scala.annotation.tailrec
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.Stepper.EfficientSplit
import scala.math.Ordering

/** Base trait for indexed sequences that have efficient `apply` and `length` */
trait IndexedSeq[+A] extends Seq[A]
  with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]
  with IterableFactoryDefaults[A, IndexedSeq] {
  override protected[this] def stringPrefix: String = "IndexedSeq"

  override def iterableFactory: SeqFactory[IndexedSeq] = IndexedSeq
}

@SerialVersionUID(3L)
object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](immutable.IndexedSeq)

/** Base trait for indexed Seq operations */
trait IndexedSeqOps[+A, +CC[_], +C] extends Any with SeqOps[A, CC, C] { self =>

  def iterator: Iterator[A] = view.iterator

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S with EfficientSplit = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntIndexedSeqStepper   (this.asInstanceOf[IndexedSeqOps[Int, AnyConstr, _]],    0, length)
      case StepperShape.LongShape   => new LongIndexedSeqStepper  (this.asInstanceOf[IndexedSeqOps[Long, AnyConstr, _]],   0, length)
      case StepperShape.DoubleShape => new DoubleIndexedSeqStepper(this.asInstanceOf[IndexedSeqOps[Double, AnyConstr, _]], 0, length)
      case _                        => shape.parUnbox(new AnyIndexedSeqStepper[A](this, 0, length))
    }
    s.asInstanceOf[S with EfficientSplit]
  }

  override def reverseIterator: Iterator[A] = new AbstractIterator[A] {
    private[this] var i = self.length
    def hasNext: Boolean = 0 < i
    def next(): A =
      if (0 < i) {
        i -= 1
        self(i)
      } else Iterator.empty.next()
  }

  override def foldRight[B](z: B)(op: (A, B) => B): B = {
    val it = reverseIterator
    var b = z
    while (it.hasNext)
      b = op(it.next(), b)
    b
  }

  override def view: IndexedSeqView[A] = new IndexedSeqView.Id[A](this)

  @deprecated("Use .view.slice(from, until) instead of .view(from, until)", "2.13.0")
  override def view(from: Int, until: Int): IndexedSeqView[A] = view.slice(from, until)

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

  override def sliding(size: Int, step: Int): Iterator[C] =
    new IndexedSeqSlidingIterator[A, CC, C](this, size, step)

  override def head: A = apply(0)

  override def headOption: Option[A] = if (isEmpty) None else Some(head)

  override def last: A = apply(length - 1)

  // We already inherit an efficient `lastOption = if (isEmpty) None else Some(last)`

  override final def lengthCompare(len: Int): Int = Integer.compare(length, len)

  override def knownSize: Int = length

  override final def lengthCompare(that: Iterable[_]): Int = {
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

/** A fast sliding iterator for IndexedSeqs which uses the underlying `slice` operation. */
private final class IndexedSeqSlidingIterator[A, CC[_], C](s: IndexedSeqOps[A, CC, C], size: Int, step: Int)
  extends Iterator[C] {

  private[this] val len = s.length
  private[this] var pos = 0

  def hasNext: Boolean = pos < len

  def next(): C = {
    if(!hasNext) Iterator.empty.next()
    val end = ((pos.toLong + size) min len).toInt
    val slice = s.slice(pos, end)
    if(end >= len) pos = len
    else pos = ((pos.toLong + step) min len).toInt
    slice
  }
}
