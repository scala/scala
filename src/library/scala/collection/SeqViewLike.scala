/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

import generic._
import Seq.fill
import TraversableView.NoBuilder

/** A template trait for non-strict views of sequences.
 *  $seqViewInfo
 *
 *  @define seqViewInfo
 *  $viewInfo
 *  All views for sequences are defined by re-interpreting the `length` and `apply` methods.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @tparam A    the element type of the view
 *  @tparam Coll the type of the underlying collection containing the elements.
 *  @tparam This the type of the view itself
 */
trait SeqViewLike[+A,
                  +Coll,
                  +This <: SeqView[A, Coll] with SeqViewLike[A, Coll, This]]
  extends Seq[A] with SeqLike[A, This] with IterableView[A, Coll] with IterableViewLike[A, Coll, This]
{ self =>

  trait Transformed[+B] extends SeqView[B, Coll] with super.Transformed[B] {
    def length: Int
    def apply(idx: Int): B
    override def toString = viewToString
  }

  trait EmptyView extends Transformed[Nothing] with super.EmptyView {
    final override def length = 0
    final override def apply(n: Int) = Nil(n)
  }

  trait Forced[B] extends super.Forced[B] with Transformed[B] {
    def length = forced.length
    def apply(idx: Int) = forced.apply(idx)
  }

  trait Sliced extends super.Sliced with Transformed[A] {
    def length = iterator.size
    def apply(idx: Int): A =
      if (idx + from < until) self.apply(idx + from)
      else throw new IndexOutOfBoundsException(idx.toString)

    override def foreach[U](f: A => U) = iterator foreach f
    override def iterator: Iterator[A] = self.iterator drop from take endpoints.width
  }

  trait Mapped[B] extends super.Mapped[B] with Transformed[B] {
    def length = self.length
    def apply(idx: Int): B = mapping(self(idx))
  }

  trait FlatMapped[B] extends super.FlatMapped[B] with Transformed[B] {
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
    def length = index(self.length)
    def apply(idx: Int) = {
      val row = findRow(idx, 0, self.length - 1)
      mapping(self(row)).toSeq(idx - index(row))
    }
  }

  trait Appended[B >: A] extends super.Appended[B] with Transformed[B] {
    protected[this] lazy val restSeq = rest.toSeq
    def length = self.length + restSeq.length
    def apply(idx: Int) =
      if (idx < self.length) self(idx) else restSeq(idx - self.length)
  }

  trait Filtered extends super.Filtered with Transformed[A] {
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
    def length = index.length
    def apply(idx: Int) = self(index(idx))
  }

  trait TakenWhile extends super.TakenWhile with Transformed[A] {
    protected[this] lazy val len = self prefixLength pred
    def length = len
    def apply(idx: Int) =
      if (idx < len) self(idx)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait DroppedWhile extends super.DroppedWhile with Transformed[A] {
    protected[this] lazy val start = self prefixLength pred
    def length = self.length - start
    def apply(idx: Int) =
      if (idx >= 0) self(idx + start)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait Zipped[B] extends super.Zipped[B] with Transformed[(A, B)] {
    protected[this] lazy val thatSeq = other.toSeq
    /* Have to be careful here - other may be an infinite sequence. */
    def length = if ((thatSeq lengthCompare self.length) <= 0) thatSeq.length else self.length
    def apply(idx: Int) = (self.apply(idx), thatSeq.apply(idx))
  }

  trait ZippedAll[A1 >: A, B] extends super.ZippedAll[A1, B] with Transformed[(A1, B)] {
    protected[this] lazy val thatSeq = other.toSeq
    def length: Int = self.length max thatSeq.length
    def apply(idx: Int) =
      (if (idx < self.length) self.apply(idx) else thisElem,
       if (idx < thatSeq.length) thatSeq.apply(idx) else thatElem)
  }

  trait Reversed extends Transformed[A] {
    override def iterator: Iterator[A] = createReversedIterator
    def length: Int = self.length
    def apply(idx: Int): A = self.apply(length - 1 - idx)
    final override protected[this] def viewIdentifier = "R"

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
    def length: Int = self.length + plen - replaced
    def apply(idx: Int): B =
      if (idx < from) self.apply(idx)
      else if (idx < from + plen) patch.apply(idx - from)
      else self.apply(idx - plen + replaced)
    final override protected[this] def viewIdentifier = "P"
  }

  trait Prepended[B >: A] extends Transformed[B] {
    protected[this] val fst: B
    override def iterator: Iterator[B] = Iterator.single(fst) ++ self.iterator
    def length: Int = 1 + self.length
    def apply(idx: Int): B =
      if (idx == 0) fst
      else self.apply(idx - 1)
    final override protected[this] def viewIdentifier = "A"
  }

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected override def newForced[B](xs: => Seq[B]): Transformed[B] = new { val forced = xs } with Forced[B]
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new { val rest = that } with Appended[B]
  protected override def newMapped[B](f: A => B): Transformed[B] = new { val mapping = f } with Mapped[B]
  protected override def newFlatMapped[B](f: A => TraversableOnce[B]): Transformed[B] = new { val mapping = f } with FlatMapped[B]
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new { val pred = p } with Filtered
  protected override def newSliced(_endpoints: SliceInterval): Transformed[A] = new { val endpoints = _endpoints } with Sliced
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with DroppedWhile
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with TakenWhile
  protected override def newZipped[B](that: Iterable[B]): Transformed[(A, B)] = new { val other = that } with Zipped[B]
  protected override def newZippedAll[A1 >: A, B](that: Iterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = new {
    val other = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  } with ZippedAll[A1, B]
  protected def newReversed: Transformed[A] = new Reversed { }
  protected def newPatched[B >: A](_from: Int, _patch: Seq[B], _replaced: Int): Transformed[B] = new { val from = _from; val patch = _patch; val replaced = _replaced } with Patched[B]
  protected def newPrepended[B >: A](elem: B): Transformed[B] = new { protected[this] val fst = elem } with Prepended[B]

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
    reverse map f

  override def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[This, B, That]): That = {
    require(0 <= index && index < length) // !!! can't call length like this.
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


