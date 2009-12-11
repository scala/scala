/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import mutable.ArrayBuffer
import scala.annotation.tailrec

/** A template trait for indexed sequences of type `IndexedSeq[A]`.
 *
 *  $indexedSeqInfo
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @define indexedSeqInfo
 *  Indexed sequences support constant-time or near constant-time element
 *  access and length computation.
 *
 *  Indexed sequences do not define any new methods wrt `Seq`. However, some `Seq` methods
 *  are overridden with optimized implementations.
 *
 *  @tparam A    the element type of the $coll
 *  @tparam Repr the type of the actual $coll containing the elements.
 */
trait IndexedSeqLike[+A, +Repr] extends SeqLike[A, Repr] { self =>

  override protected[this] def thisCollection: IndexedSeq[A] = this.asInstanceOf[IndexedSeq[A]]
  override protected[this] def toCollection(repr: Repr): IndexedSeq[A] = repr.asInstanceOf[IndexedSeq[A]]

  /** The class of the iterator returned by the `iterator` method.
   *  multiple `take`, `drop`, and `slice` operations on this iterator are bunched
   *  together for better efficiency.
   */
  @serializable @SerialVersionUID(1756321872811029277L)
  protected class Elements(start: Int, end: Int) extends BufferedIterator[A] {
    private var i = start

    def hasNext: Boolean = i < end

    def next: A =
      if (i < end) {
        val x = self(i)
        i += 1
        x
      } else Iterator.empty.next

    def head =
      if (i < end) self(i) else Iterator.empty.next

    /** $super
     *  @note `drop` is overridden to enable fast searching in the middle of indexed sequences.
     */
    override def drop(n: Int): Iterator[A] =
      if (n > 0) new Elements(start + n, end) else this

    /** $super
     *  @note `take` is overridden to be symmetric to `drop`.
     */
    override def take(n: Int): Iterator[A] =
      if (n <= 0) Iterator.empty.buffered
      else if (start + n < end) new Elements(start, start + n)
      else this
  }

  override /*IterableLike*/
  def iterator: Iterator[A] = new Elements(0, length)

  override /*IterableLike*/
  def isEmpty: Boolean = { length == 0 }

  override /*IterableLike*/
  def foreach[U](f: A =>  U): Unit = {
    var i = 0
    val len = length
    while (i < len) { f(this(i)); i += 1 }
  }

  override /*IterableLike*/
  def forall(p: A => Boolean): Boolean = prefixLength(p(_)) == length

  override /*IterableLike*/
  def exists(p: A => Boolean): Boolean = prefixLength(!p(_)) != length

  override /*IterableLike*/
  def find(p: A => Boolean): Option[A] = {
    val i = prefixLength(!p(_))
    if (i < length) Some(this(i)) else None
  }
/*
  override /*IterableLike*/
  def mapFind[B](f: A => Option[B]): Option[B] = {
    var i = 0
    var res: Option[B] = None
    val len = length
    while (res.isEmpty && i < len) {
      res = f(this(i))
      i += 1
    }
    res
  }
*/
  @tailrec
  private def foldl[B](start: Int, end: Int, z: B, op: (B, A) => B): B =
    if (start == end) z
    else foldl(start + 1, end, op(z, this(start)), op)

  @tailrec
  private def foldr[B](start: Int, end: Int, z: B, op: (A, B) => B): B =
    if (start == end) z
    else foldr(start, end - 1, op(this(end - 1), z), op)

  override /*TraversableLike*/
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    foldl(0, length, z, op)

  override /*IterableLike*/
  def foldRight[B](z: B)(op: (A, B) => B): B =
    foldr(0, length, z, op)

  override /*TraversableLike*/
  def reduceLeft[B >: A](op: (B, A) => B): B =
    if (length > 0) foldl(1, length, this(0), op) else super.reduceLeft(op)

  override /*IterableLike*/
  def reduceRight[B >: A](op: (A, B) => B): B =
    if (length > 0) foldr(0, length - 1, this(length - 1), op) else super.reduceRight(op)

  override /*IterableLike*/
  def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = that match {
    case that: IndexedSeq[_] =>
      val b = bf(repr)
      var i = 0
      val len = this.length min that.length
      b.sizeHint(len)
      while (i < len) {
        b += ((this(i), that(i).asInstanceOf[B]))
        i += 1
      }
      b.result
    case _ =>
      super.zip[A1, B, That](that)(bf)
  }

  override /*IterableLike*/
  def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, Int), That]): That = {
    val b = bf(repr)
    val len = length
    b.sizeHint(len)
    var i = 0
    while (i < len) {
      b += ((this(i), i))
      i += 1
    }
    b.result
  }

  override /*IterableLike*/
  def slice(from: Int, until: Int): Repr = {
    var i = from max 0
    val end = until min length
    val b = newBuilder
    b.sizeHint(end - i)
    while (i < end) {
      b += this(i)
      i += 1
    }
    b.result
  }

  override /*IterableLike*/
  def head: A = if (isEmpty) super.head else this(0)

  override /*TraversableLike*/
  def tail: Repr = if (isEmpty) super.tail else slice(1, length)

  override /*TraversableLike*/
  def last: A = if (length > 0) this(length - 1) else super.last

  override /*IterableLike*/
  def init: Repr = if (length > 0) slice(0, length - 1) else super.init

  override /*TraversableLike*/
  def take(n: Int): Repr = slice(0, n)

  override /*TraversableLike*/
  def drop(n: Int): Repr = slice(n, length)

  override /*IterableLike*/
  def takeRight(n: Int): Repr = slice(length - n, length)

  override /*IterableLike*/
  def dropRight(n: Int): Repr = slice(0, length - n)

  override /*TraversableLike*/
  def splitAt(n: Int): (Repr, Repr) = (take(n), drop(n))

  override /*IterableLike*/
  def takeWhile(p: A => Boolean): Repr = take(prefixLength(p))

  override /*TraversableLike*/
  def dropWhile(p: A => Boolean): Repr = drop(prefixLength(p))

  override /*TraversableLike*/
  def span(p: A => Boolean): (Repr, Repr) = splitAt(prefixLength(p))

  override /*IterableLike*/
  def sameElements[B >: A](that: Iterable[B]): Boolean = that match {
    case that: IndexedSeq[_] =>
      val len = length
      len == that.length && {
        var i = 0
        while (i < len && this(i) == that(i)) i += 1
        i == len
      }
    case _ =>
      super.sameElements(that)
  }

  override /*IterableLike*/
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
    var i = 0
    var j = start
    val end = length min len min (xs.length - start)
    while (i < end) {
      xs(j) = this(i)
      i += 1
      j += 1
    }
  }


  // Overridden methods from Seq

  override /*SeqLike*/
  def lengthCompare(len: Int): Int = length - len

  override /*SeqLike*/
  def segmentLength(p: A => Boolean, from: Int): Int = {
    val start = from
    val len = length
    var i = start
    while (i < len && p(this(i))) i += 1
    i - start
  }

  private def negLength(n: Int) = if (n == length) -1 else n

  override /*SeqLike*/
  def indexWhere(p: A => Boolean, from: Int): Int = {
    val start = from max 0
    negLength(start + segmentLength(!p(_), start))
  }

  override /*SeqLike*/
  def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = end
    while (i >= 0 && !p(this(i))) i -= 1
    i
  }

  override /*SeqLike*/
  def reverse: Repr = {
    val b = newBuilder
    b.sizeHint(length)
    var i = length
    while (0 < i) {
      i -= 1
      b += this(i)
    }
    b.result
  }

  override /*SeqLike*/
  def reverseIterator: Iterator[A] = new Iterator[A] {
    private var i = self.length
    def hasNext: Boolean = 0 < i
    def next: A =
      if (0 < i) {
        i -= 1
        self(i)
      } else Iterator.empty.next
  }

  override /*SeqLike*/
  def startsWith[B](that: Seq[B], offset: Int): Boolean = that match {
    case that: IndexedSeq[_] =>
      var i = offset
      var j = 0
      val thisLen = length
      val thatLen = that.length
      while (i < thisLen && j < thatLen && this(i) == that(j)) {
        i += 1
        j += 1
      }
      j == thatLen
    case _ =>
      var i = offset
      val thisLen = length
      val thatElems = that.iterator
      while (i < thisLen && thatElems.hasNext) {
        if (this(i) != thatElems.next())
          return false

        i += 1
      }
      !thatElems.hasNext
  }

  override /*SeqLike*/
  def endsWith[B](that: Seq[B]): Boolean = that match {
    case that: IndexedSeq[_] =>
      var i = length - 1
      var j = that.length - 1

      (j <= i) && {
        while (j >= 0) {
          if (this(i) != that(j))
            return false
          i -= 1
          j -= 1
        }
        true
      }
    case _ =>
      super.endsWith(that)
  }

  override /*SeqLike*/
  def view = new IndexedSeqView[A, Repr] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }

  override /*SeqLike*/
  def view(from: Int, until: Int) = view.slice(from, until)
}

