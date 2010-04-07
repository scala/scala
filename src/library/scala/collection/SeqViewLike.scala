/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import Seq.fill
import TraversableView.NoBuilder

/** A template trait for a non-strict view of a sequence.
 * @author Sean McDirmid
 * @author Martin Odersky
 * @version 2.8
 */
trait SeqViewLike[+A,
                  +Coll,
                  +This <: SeqView[A, Coll] with SeqViewLike[A, Coll, This]]
  extends Seq[A] with SeqLike[A, This] with IterableView[A, Coll] with IterableViewLike[A, Coll, This]
{ self =>

  trait Transformed[+B] extends SeqView[B, Coll] with super.Transformed[B] {
    override def length: Int
    override def apply(idx: Int): B
  }

  trait Forced[B] extends Transformed[B] with super.Forced[B] {
    override def length = forced.length
    override def apply(idx: Int) = forced.apply(idx)
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
      mapping(self(row)).toSeq(idx - index(row))
    }
  }

  trait Appended[B >: A] extends Transformed[B] with super.Appended[B] {
    protected[this] lazy val restSeq = rest.toSeq
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

  trait Zipped[B] extends Transformed[(A, B)] with super.Zipped[B] {
    protected[this] lazy val thatSeq = other.toSeq
    /* Have to be careful here - other may be an infinite sequence. */
    override def length = if ((thatSeq lengthCompare self.length) <= 0) thatSeq.length else self.length
    override def apply(idx: Int) = (self.apply(idx), thatSeq.apply(idx))
  }

  trait ZippedAll[A1 >: A, B] extends Transformed[(A1, B)] with super.ZippedAll[A1, B] {
    protected[this] lazy val thatSeq = other.toSeq
    override def length: Int = self.length max thatSeq.length
    override def apply(idx: Int) =
      (if (idx < self.length) self.apply(idx) else thisElem,
       if (idx < thatSeq.length) thatSeq.apply(idx) else thatElem)
  }

  trait Reversed extends Transformed[A] {
    override def iterator: Iterator[A] = createReversedIterator
    override def length: Int = self.length
    override def apply(idx: Int): A = self.apply(length - 1 - idx)
    override def stringPrefix = self.stringPrefix+"R"

    private def createReversedIterator = {
      var lst = List[A]()
      for (elem <- self) lst ::= elem
      lst.iterator
    }
  }

  trait Patched[B >: A] extends Transformed[B] {
    protected[this] val from: Int
    protected[this] val patch: Seq[B]
    protected[this] val replaced: Int
    private lazy val plen = patch.length
    override def iterator: Iterator[B] = self.iterator patch (from, patch.iterator, replaced)
    override def length: Int = self.length + plen - replaced
    override def apply(idx: Int): B =
      if (idx < from) self.apply(idx)
      else if (idx < from + plen) patch.apply(idx - from)
      else self.apply(idx - plen + replaced)
    override def stringPrefix = self.stringPrefix+"P"
  }

  trait Prepended[B >: A] extends Transformed[B] {
    protected[this] val fst: B
    override def iterator: Iterator[B] = Iterator.single(fst) ++ self.iterator
    override def length: Int = 1 + self.length
    override def apply(idx: Int): B =
      if (idx == 0) fst
      else self.apply(idx - 1)
    override def stringPrefix = self.stringPrefix+"A"
  }

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected override def newForced[B](xs: => Seq[B]): Transformed[B] = new Forced[B] { val forced = xs }
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }
  protected override def newZipped[B](that: Iterable[B]): Transformed[(A, B)] = new Zipped[B] { val other = that }
  protected override def newZippedAll[A1 >: A, B](that: Iterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = new ZippedAll[A1, B] { val other = that; val thisElem = _thisElem; val thatElem = _thatElem }
  protected def newReversed: Transformed[A] = new Reversed { }
  protected def newPatched[B >: A](_from: Int, _patch: Seq[B], _replaced: Int): Transformed[B] = new Patched[B] { val from = _from; val patch = _patch; val replaced = _replaced }
  protected def newPrepended[B >: A](elem: B): Transformed[B] = new Prepended[B] { protected[this] val fst = elem }

  override def reverse: This = newReversed.asInstanceOf[This]

  override def patch[B >: A, That](from: Int, patch: Seq[B], replaced: Int)(implicit bf: CanBuildFrom[This, B, That]): That = {
    newPatched(from, patch, replaced).asInstanceOf[That]
// was:    val b = bf(repr)
//    if (b.isInstanceOf[NoBuilder[_]]) newPatched(from, patch, replaced).asInstanceOf[That]
//    else super.patch[B, That](from, patch, replaced)(bf)
  }

  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[This, B, That]): That =
    patch(length, fill(len - length)(elem), 0)

  override def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[This, B, That]): That =
    reverse.map(f)

  override def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[This, B, That]): That = {
    require(0 <= index && index < length)
    patch(index, List(elem), 1)(bf)
  }

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[This, B, That]): That =
    newPrepended(elem).asInstanceOf[That]

  override def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[This, B, That]): That =
    ++(Iterator.single(elem))(bf)

  override def union[B >: A, That](that: Seq[B])(implicit bf: CanBuildFrom[This, B, That]): That =
    newForced(thisSeq union that).asInstanceOf[That]

  override def diff[B >: A](that: Seq[B]): This =
    newForced(thisSeq diff that).asInstanceOf[This]

  override def intersect[B >: A](that: Seq[B]): This =
    newForced(thisSeq intersect that).asInstanceOf[This]

  override def sorted[B >: A](implicit ord: Ordering[B]): This =
    newForced(thisSeq sorted ord).asInstanceOf[This]

  override def stringPrefix = "SeqView"
}


