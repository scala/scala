/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection




trait GenSeqViewLike[+A,
                     +Coll,
                     +This <: GenSeqView[A, Coll] with GenSeqViewLike[A, Coll, This]]
extends GenSeq[A] with GenSeqLike[A, This] with GenIterableView[A, Coll] with GenIterableViewLike[A, Coll, This] {
self =>

  trait Transformed[+B] extends GenSeqView[B, Coll] with super.Transformed[B] {
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
      for (i <- 0 until self.length) // note that if the mapping returns a list, performance is bad, bad
        index(i + 1) = index(i) + mapping(self(i)).seq.size
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
      mapping(self(row)).seq.toSeq(idx - index(row))
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
    protected[this] lazy val thatSeq = other.seq.toSeq
    /* Have to be careful here - other may be an infinite sequence. */
    def length = if ((thatSeq lengthCompare self.length) <= 0) thatSeq.length else self.length
    def apply(idx: Int) = (self.apply(idx), thatSeq.apply(idx))
  }

  trait ZippedAll[A1 >: A, B] extends super.ZippedAll[A1, B] with Transformed[(A1, B)] {
    protected[this] lazy val thatSeq = other.seq.toSeq
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
    protected[this] val patch: GenSeq[B]
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

}


