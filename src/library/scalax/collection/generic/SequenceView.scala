/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Sequence.scala 16092 2008-09-12 10:37:06Z nielsen $


package scalax.collection.generic

import util.control.Breaks._
import annotation.unchecked.uncheckedVariance

import Sequence._

/** A non-strict projection of an iterable.
 * @author Sean McDirmid
 * @author Martin Odersky
 * @note this should really be a virtual class of SequenceFactory
 */
trait SequenceView[+UC[/*+*/B] <: Sequence[B], /*+*/A] extends IterableView[UC, A] with Sequence[A] {
self =>

  /** refined from Iterable.View */
  val origin: Sequence[_]

  trait Transformed[/*+*/B] extends SequenceView[UC, B] {
    val origin = self
    protected def asCC = asInstanceOf[SequenceView[UC, B]]
  }

  class Appended[/*+*/B >: A](that: Sequence[B]) extends Transformed[B] {
    override def elements: Iterator[B] = self.elements ++ that.elements
    override def length: Int = self.length + that.length
    override def apply(idx: Int): B = {
      val ll = self.length
      if (idx < ll) self.apply(idx) else that.apply(idx - ll)
    }
    override def stringPrefix = self.stringPrefix + "A"
  }

  class Sliced(from: Int, to: Int) extends Transformed[A] {
    override def elements: Iterator[A] = self.elements slice (from, to)
    override lazy val length: Int = ((to min self.length) - (from max 0) min 0)
    override def apply(idx: Int): A =
      if (idx >= 0 && idx < length) self.apply(idx + from)
      else throw new IndexOutOfBoundsException(idx.toString)
    override def stringPrefix = self.stringPrefix + "S"
    override def slice(from1: Int, to1: Int) =
      new self.Sliced(from + (from1 min length max 0) , to + (to1 min length max 0)).asInstanceOf[SequenceView[UC, A]]
  }

  class Mapped[/*+*/B](f: A => B) extends Transformed[B] {
    override def elements: Iterator[B] = self.elements map f
    override def length: Int = self.length
    override def apply(idx: Int): B = f(self.apply(idx))
    override def stringPrefix = self.stringPrefix + "M"
  }

  class Reversed extends Transformed[A] {
    override def elements: Iterator[A] = self.reversedElements
    override def length: Int = self.length
    override def apply(idx: Int): A = self.apply(length - 1 - idx)
    override def stringPrefix = super.stringPrefix+"R"
  }

  class Patched[/*+*/B >: A](from: Int, patch: Sequence[B], replaced: Int) extends Transformed[B] {
    val plen = patch.length
    override def elements: Iterator[B] = self.elements patch (from, patch, replaced)
    override def length: Int = self.length + plen - replaced
    override def apply(idx: Int): B =
      if (idx < from) self.apply(idx)
      else if (idx < from + plen) patch.apply(idx - from)
      else self.apply(idx - plen + replaced)
    override def stringPrefix = super.stringPrefix+"P"
  }

  class Zipped[/*+*/B](that: Sequence[B]) extends Transformed[(A, B)] {
    override def elements: Iterator[(A, B)] = self.elements zip that.elements
    override def length = self.length min that.length
    override def apply(idx: Int): (A, B) = (self.apply(idx), that.apply(idx))
    override def stringPrefix = super.stringPrefix+"Z"
  }

  override def ++[B >: A](that: Iterable[B]): SequenceView[UC, B] =
    new Appended[B](that.toSequence).asCC
  override def ++[B >: A](that: Iterator[B]): SequenceView[UC, B] =
    new Appended[B](that.toSequence).asCC
  override def map[B](f: A => B): SequenceView[UC, B] =
    new Mapped(f).asCC
  override def reverse: SequenceView[UC, A] =
    (new Reversed).asCC
  def patch[B >: A](from: Int, patch: Sequence[B], replaced: Int): SequenceView[UC, B] =
    if (0 <= from && from < length) new Patched(from, patch, replaced).asCC
    else throw new IndexOutOfBoundsException(from.toString)
  override def padTo[B >: A](len: Int, elem: B): SequenceView[UC, B] =
    patch(length, fill((len - length) max 0)(elem), 0)
  override def zip[B](that: Iterable[B]): SequenceView[UC, (A, B)] =
    new Zipped(that.toSequence).asCC
  override def zipWithIndex: SequenceView[UC, (A, Int)] =
    zip((0 until length).asInstanceOf[Null]) // !@!
    /*
  override def zipAll[B, A1 >: A, B1 >: B](that: Iterable[B], thisElem: A1, thatElem: B1): SequenceView[UC, (A1, B1)] = {
    val that1 = that.toSequence
    self.padTo(that1.length, thisElem) zip that1.padTo(this.length, thatElem)//.asInstanceOf[SequenceView[UC, (A1, B1)]]
  }*/
  override def take(n: Int): SequenceView[UC, A] =
    slice(0, n)
  override def drop(n: Int): SequenceView[UC, A] =
    slice(n, Math.MAX_INT)
  override def splitAt(n: Int): (SequenceView[UC, A], SequenceView[UC, A]) = (take(n), drop(n))
  override def slice(from: Int, until: Int): SequenceView[UC, A] =
    new Sliced(from, until).asCC
  override def takeWhile(p: A => Boolean): SequenceView[UC, A] =
    take(prefixLength(p))
  override def dropWhile(p: A => Boolean): SequenceView[UC, A] =
    drop(prefixLength(p))
  override def span(p: A => Boolean): (SequenceView[UC, A], SequenceView[UC, A]) = {
    val n = prefixLength(p)
    (take(n), drop(n))
  }

  // missing here because we can't do anything about them, so we fall back to default construction
  // if an IterableView via newView: flatMap, filter, partition
}

