/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import mutable.ArrayBuffer

/** Sequences that support O(1) element access and O(1) length computation.
 *  This class does not add any methods to Seq but overrides several
 *  methods with optimized implementations.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait VectorLike[+A, +Repr] extends SeqLike[A, Repr] { self =>

  override protected[this] def thisCollection: Vector[A] = this.asInstanceOf[Vector[A]]
  override protected[this] def toCollection(repr: Repr): Vector[A] = repr.asInstanceOf[Vector[A]]

  // Overridden methods from IterableLike

  /** The iterator returned by the iterator method
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

    /** drop is overridden to enable fast searching in the middle of random access sequences */
    override def drop(n: Int): Iterator[A] =
      if (n > 0) new Elements(start + n, end) else this

    /** take is overridden to be symmetric to drop */
    override def take(n: Int): Iterator[A] =
      if (n <= 0) Iterator.empty.buffered
      else if (start + n < end) new Elements(start, start + n)
      else this
  }

  override def iterator: Iterator[A] = new Elements(0, length)

  override def isEmpty: Boolean = { length == 0 }

  override def foreach[U](f: A =>  U): Unit = {
    var i = 0
    val len = length
    while (i < len) { f(this(i)); i += 1 }
  }

  override def forall(p: A => Boolean): Boolean = prefixLength(p(_)) == length
  override def exists(p: A => Boolean): Boolean = prefixLength(!p(_)) != length

  override def find(p: A => Boolean): Option[A] = {
    val i = prefixLength(!p(_))
    if (i < length) Some(this(i)) else None
  }

  private def foldl[B](start: Int, z: B, op: (B, A) => B): B = {
    var i = start
    val len = length
    var result = z
    while (i < len) {
      result = op(result, this(i))
      i += 1
    }
    result
  }

  private def foldr[B](start: Int, len: Int, z: B, op: (A, B) => B): B =
    if (start == len) z
    else op(this(start), foldr(start + 1, len, z, op))

  override def foldLeft[B](z: B)(op: (B, A) => B): B =
    foldl(0, z, op)
  override def foldRight[B](z: B)(op: (A, B) => B): B =
    foldr(0, length, z, op)
  override def reduceLeft[B >: A](op: (B, A) => B): B =
    if (length > 0) foldl(1, this(0), op) else super.reduceLeft(op)
  override def reduceRight[B >: A](op: (A, B) => B): B =
    if (length > 0) foldr(0, length - 1, this(length - 1), op) else super.reduceRight(op)

  override def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: BuilderFactory[(A1, B), That, Repr]): That = that match {
    case that: Vector[_] =>
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

  override def zipWithIndex[A1 >: A, That](implicit bf: BuilderFactory[(A1, Int), That, Repr]): That = {
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

  override def slice(from: Int, until: Int): Repr = {
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

  override def head: A = if (isEmpty) super.head else this(0)
  override def tail: Repr = if (isEmpty) super.tail else slice(1, length)
  override def last: A = if (length > 0) this(length - 1) else super.last
  override def init: Repr = if (length > 0) slice(0, length - 1) else super.init
  override def take(n: Int): Repr = slice(0, n)
  override def drop(n: Int): Repr = slice(n, length)
  override def takeRight(n: Int): Repr = slice(length - n, length)
  override def dropRight(n: Int): Repr = slice(0, length - n)
  override def splitAt(n: Int): (Repr, Repr) = (take(n), drop(n))
  override def takeWhile(p: A => Boolean): Repr = take(prefixLength(p))
  override def dropWhile(p: A => Boolean): Repr = drop(prefixLength(p))
  override def span(p: A => Boolean): (Repr, Repr) = splitAt(prefixLength(p))

  override def sameElements[B >: A](that: Iterable[B]): Boolean = that match {
    case that: Vector[_] =>
      val len = length
      len == that.length && {
        var i = 0
        while (i < len && this(i) == that(i)) i += 1
        i == len
      }
    case _ =>
      super.sameElements(that)
  }

  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
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

  override def lengthCompare(len: Int): Int = length - len

  override def segmentLength(p: A => Boolean, from: Int): Int = {
    val start = from
    val len = length
    var i = start
    while (i < len && p(this(i))) i += 1
    i - start
  }

  private def negLength(n: Int) = if (n == length) -1 else n

  override def indexWhere(p: A => Boolean, from: Int): Int = {
    val start = from max 0
    negLength(start + segmentLength(!p(_), start))
  }

  override def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = end
    while (i >= 0 && !p(this(i))) i -= 1
    i
  }

  override def reverse: Repr = {
    val b = newBuilder
    b.sizeHint(length)
    var i = length
    while (0 < i) {
      i -= 1
      b += this(i)
    }
    b.result
  }

  override def reverseIterator: Iterator[A] = new Iterator[A] {
    private var i = self.length
    def hasNext: Boolean = 0 < i
    def next: A =
      if (0 < i) {
        i -= 1
        self(i)
      } else Iterator.empty.next
  }

  override def startsWith[B](that: Seq[B], offset: Int): Boolean = that match {
    case that: Vector[_] =>
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

  override def endsWith[B](that: Seq[B]): Boolean = that match {
    case that: Vector[_] =>
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

  override def view = new VectorView[A, Repr] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }

  override def view(from: Int, until: Int) = view.slice(from, until)
}

