/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic
import scala.collection._

import Sequence.fill
import TraversableView.NoBuilder

/** A non-strict projection of an iterable.
 * @author Sean McDirmid
 * @author Martin Odersky
 * @version 2.8
 */
trait SequenceViewTemplate[+A,
                           +Coll <: Sequence[_],
                           +This <: SequenceView[A, Coll] with SequenceViewTemplate[A, Coll, This]]
  extends Sequence[A] with SequenceTemplate[A, This] with IterableView[A, Coll] with IterableViewTemplate[A, Coll, This]
{ self =>

  trait Transformed[+B] extends SequenceView[B, Coll] with super.Transformed[B] {
    override def length: Int
    override def apply(idx: Int): B
  }

  trait Sliced extends Transformed[A] with super.Sliced {
    override def length = ((until min self.length) - from) max 0
    override def apply(idx: Int): A =
      if (idx + from < until) self.apply(idx + from)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait Mapped[B] extends Transformed[B] with super.Mapped[B] {
    override def length = self.length
    override def apply(idx: Int): B = mapping(self apply idx)
  }

  trait FlatMapped[B] extends Transformed[B] with super.FlatMapped[B] {
    protected[this] lazy val index = {
      val index = new Array[Int](self.length + 1)
      index(0) = 0
      for (i <- 0 until self.length)
        index(i + 1) = index(i) + mapping(self(i)).size
      index
    }
    protected[this] def findRow(idx: Int, lo: Int, hi: Int): Int = {
      val mid = (lo + hi) / 2
      if (idx < index(mid)) findRow(idx, lo, mid - 1)
      else if (idx >= index(mid + 1)) findRow(idx, mid + 1, hi)
      else mid
    }
    override def length = index(self.length)
    override def apply(idx: Int) = {
      val row = findRow(idx, 0, self.length - 1)
      mapping(self(row)).toSequence(idx - index(row))
    }
  }

  trait Appended[B >: A] extends Transformed[B] with super.Appended[B] {
    lazy val restSeq = rest.toSequence
    override def length = self.length + restSeq.length
    override def apply(idx: Int) =
      if (idx < self.length) self(idx) else restSeq(idx - self.length)
  }

  trait Filtered extends Transformed[A] with super.Filtered {
    protected[this] lazy val index = {
      var len = 0
      val arr = new Array[Int](self.length)
      for (i <- 0 until self.length)
        if (pred(self(i))) {
          arr(len) = i
          len += 1
        }
      arr take len
    }
    override def length = index.length
    override def apply(idx: Int) = self(index(idx))
  }

  trait TakenWhile extends Transformed[A] with super.TakenWhile {
    protected[this] lazy val len = self prefixLength pred
    override def length = len
    override def apply(idx: Int) =
      if (idx < len) self(idx)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait DroppedWhile extends Transformed[A] with super.DroppedWhile {
    protected[this] lazy val start = self prefixLength pred
    override def length = self.length - start
    override def apply(idx: Int) =
      if (idx >= 0) self(idx + start)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait Reversed extends Transformed[A] {
    override def iterator: Iterator[A] = self.reverseIterator
    override def length: Int = self.length
    override def apply(idx: Int): A = self.apply(length - 1 - idx)
    override def stringPrefix = super.stringPrefix+"R"
  }

  trait Patched[B >: A] extends Transformed[B] {
    protected[this] val from: Int
    protected[this] val patch: Sequence[B]
    protected[this] val replaced: Int
    private lazy val plen = patch.length
    override def iterator: Iterator[B] = self.iterator patch (from, patch.iterator, replaced)
    override def length: Int = self.length + plen - replaced
    override def apply(idx: Int): B =
      if (idx < from) self.apply(idx)
      else if (idx < from + plen) patch.apply(idx - from)
      else self.apply(idx - plen + replaced)
    override def stringPrefix = super.stringPrefix+"P"
  }

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }
  protected def newReversed: Transformed[A] = new Reversed { }
  protected def newPatched[B >: A](_from: Int, _patch: Sequence[B], _replaced: Int): Transformed[B] = new Patched[B] {
    val from = _from; val patch = _patch; val replaced = _replaced
  }

  override def reverse: This = newReversed.asInstanceOf[This]

  override def patch[B >: A, That](from: Int, patch: Sequence[B], replaced: Int)(implicit bf: BuilderFactory[B, That, This]): That = {
    newPatched(from, patch, replaced).asInstanceOf[That]
// was:    val b = bf(thisCollection)
//    if (b.isInstanceOf[NoBuilder[_]]) newPatched(from, patch, replaced).asInstanceOf[That]
//    else super.patch[B, That](from, patch, replaced)(bf)
  }

  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: BuilderFactory[B, That, This]): That =
    patch(length, fill(len - length)(elem), 0)
}


