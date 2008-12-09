/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Vector.scala 15437 2008-06-25 16:22:45Z stepancheg $

package scalax.collection.generic.covartest

import Vector._

/** Sequences that support O(1) element access and O(1) length computation.
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 */
trait VectorTemplate[+CC[+B] <: VectorTemplate[CC, B] with Vector[B], +A] extends SequenceTemplate[CC, A] {
self =>

  // Overridden methods from IterableTemplate

  /** The iterator returned by the elements method
   */
  protected class Elements(start: Int, end: Int) extends scalax.collection.BufferedIterator[A] {
    var i = start
    def hasNext: Boolean = i < end
    def next: A =
      if (i < end) {
        val x = self(i)
        i += 1
        x
      } else Iterator.empty.next
    def head =
      self(i)
    /** drop is overridden to enable fast searching in the middle of random access sequences */
    override def drop(n: Int): Iterator[A] =
      if (n > 0) new Elements(start + n, end) else this
    /**  is overridden to be symmetric to drop */
    override def take(n: Int): Iterator[A] =
      if (n < 0) Iterator.empty.buffered
      else if (start + n < end) new Elements(start, start + n)
      else this
  }

  override def elements: Iterator[A] = new Elements(0, length)

  override def isEmpty: Boolean = { length == 0 }

  override def foreach(f: A => Unit): Unit = {
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

  override def foldLeft[B](z: B)(op: (B, A) => B): B = foldl(0, z, op)
  override def foldRight[B](z: B)(op: (A, B) => B): B = foldr(0, length, z, op)
  override def reduceLeft[B >: A](op: (B, A) => B): B =
    if (length > 0) foldl(0, this(0), op) else super.reduceLeft(op)
  override def reduceRight[B >: A](op: (A, B) => B): B =
    if (length > 0) foldr(0, length - 1, this(length - 1), op) else super.reduceRight(op)

  override def zip[B](that: Iterable[B]): CC[(A, B)] = that match {
    case that: Vector[_] =>
      var i = 0
      val len = this.length min that.length
      val b = this.newBuilder[(A, B)]
      while (i < len) {
        b += ((this(i), that(i).asInstanceOf[B]))
        i += 1
      }
      b.result
    case _ =>
      super.zip(that)
  }

  override def zipAll[B, A1 >: A, B1 >: B](that: Iterable[B], thisElem: A1, thatElem: B1): CC[(A1, B1)] = that match {
    case that: Vector[_] =>
      var i = 0
      val len = this.length min that.length
      val b = this.newBuilder[(A1, B1)]
      while (i < len) {
        b += ((this(i), that(i).asInstanceOf[B]))
        i += 1
      }
      while (i < this.length) {
        b += ((this(i), thatElem))
        i += 1
      }
      while (i < that.length) {
        b += ((this(i), thatElem.asInstanceOf[B]))
        i += 1
      }
      b.result
    case _ =>
      super.zipAll(that, thisElem, thatElem)
  }

  override def zipWithIndex: CC[(A, Int)] = {
    val b = newBuilder[(A, Int)]
    var i = 0
    val len = length
    while (i < len) {
      b += ((this(i), i))
      i += 1
    }
    b.result
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

  // Overridden methods from OrderedIterableTemplate

  override def head: A = if (isEmpty) throw new NoSuchElementException else this(0)

  override def slice(from: Int, until: Int): CC[A] = {
    val b = newBuilder[A]
    var i = from max 0
    val end = until min length
    while (i < end) {
      b += this(i)
      i += 1
    }
    b.result
  }

  override def take(n: Int): CC[A] = slice(0, n)
  override def drop(n: Int): CC[A] = slice(n, length)
  override def takeRight(n: Int): CC[A] = slice(length - n, length)
  override def dropRight(n: Int): CC[A] = slice(0, length - n)
  override def splitAt(n: Int): (CC[A], CC[A]) = (take(n), drop(n))
  override def takeWhile(p: A => Boolean): CC[A] = take(prefixLength(p))
  override def dropWhile(p: A => Boolean): CC[A] = drop(prefixLength(p))
  override def span(p: A => Boolean): (CC[A], CC[A]) = splitAt(prefixLength(p))
  override def last: A = if (length > 0) this(length - 1) else super.last
  override def init: CC[A] = if (length > 0) slice(0, length - 1) else super.init

  override def sameElements[B >: A](that: OrderedIterable[B]): Boolean = that match {
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

  // Overridden methods from Sequence

  override def lengthCompare(len: Int): Int = length - len

  private def negLength(n: Int) = if (n == length) -1 else n

  override def indexWhere(p: A => Boolean, from: Int): Int =
    negLength(from + segmentLength(!p(_), from))

  override def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = end
    while (i >= 0 && !p(this(i))) i -= 1
    i
  }

  override def lastIndexOf[B >: A](elem: B, end: Int): Int = lastIndexWhere(elem ==, end)

  override def reverse: CC[A] = {
    val b = newBuilder[A]
    var i = length
    while (0 < i) {
      i -= 1
      b += this(i)
    }
    b.result
  }

  override def reversedElements: Iterator[A] = new Iterator[A] {
    var i = length
    def hasNext: Boolean = 0 < i
    def next: A =
      if (0 < i) {
        i -= 1
        self(i)
      } else Iterator.empty.next
  }

  override def startsWith[B](that: Sequence[B], offset: Int): Boolean = that match {
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
      val thatElems = that.elements
      while (i < thisLen && thatElems.hasNext && this(i) == thatElems.next) {
        i += 1
      }
      !thatElems.hasNext
  }

  override def endsWith[B](that: Sequence[B]): Boolean = that match {
    case that: Vector[_] =>
      val thisLen = length
      val thatLen = that.length
      var i = thisLen - 1
      var j = thatLen - 1
      while (i >= 0 && j >= 0 && this(i) == that(j)) {
        i -= 1
        j -= 1
      }
      j == 0
    case _ =>
      super.endsWith(that)
  }

  override def indexOf[B >: A](that: Sequence[B]): Int = {
    var i = 0
    val last = length - that.length
    while (i <= last && !startsWith(that, i)) i += 1
    negLength(i)
  }
}

