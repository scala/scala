/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import Predef._
import collection.mutable.ArrayBuffer

object Seq {

  /** The empty sequence */
  val empty = new Seq[Nothing] {
    def length = 0
    def apply(i: Int): Nothing = throw new NoSuchElementException("empty sequence")
    override def elements = Iterator.empty
  }

  /** This method is called in a pattern match { case Seq(...) => }.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in an option, if this is a Seq, otherwise none
   */
  def unapplySeq[A](x: Seq[A]): Option[Seq[A]] = Some(x)

  /** Builds a singleton sequence.
   *
   *  @param x ...
   *  @return  ...
   */
  def single[A](x: A) = new Seq[A] {
    def length = 1
    override def elements = Iterator.single(x)
    override def isDefinedAt(x: Int): Boolean = (x == 0)
    def apply(i: Int) = x // caller's responsibility to check isDefinedAt
  }
/*
  implicit def view[A <% Ordered[A]](xs: Seq[A]): Ordered[Seq[A]] =
    new Ordered[Seq[A]] with Proxy {
      def self: Any = xs;
      def compare[B >: Seq[A] <% Ordered[B]](that: B): Int = that match {
        case ys: Seq[A] =>
          var res = 0;
          val xsit = xs.elements;
          val ysit = ys.elements;
          while (xsit.hasNext && ysit.hasNext && (res == 0)) {
            res = xsit.next compare ysit.next;
          }
          if (res != 0) res else if (xsit.hasNext) 1 else -1
        case _ =>
          -(that compare xs)
      }
    }
*/
  trait Projection[+A] extends Seq[A] with Iterable.Projection[A]  {
    override def projection = this
    override def map[B](f: A => B) : Projection[B] = new MapProjection(f)
    protected class MapProjection[B](f : A => B) extends Projection[B] {
      def length = Projection.this.length
      def elements = Projection.this.elements.map(f)
      def apply(idx : Int) = f(Projection.this.apply(idx))
      override def stringPrefix = Projection.this.stringPrefix + "M"
    }
    override def flatMap[B](f: A => Iterable[B]): Projection[B] = new Projection[B] {
      override def stringPrefix = Projection.this.stringPrefix + "G"
      def elements = Projection.this.elements.flatMap(a => f(a).elements)
      def length = {
        var sz = 0
        Projection.this.foreach(a => sz = sz + f(a).asInstanceOf[Collection[B]].size)
        sz
      }
      def apply(idx : Int) : B = {
        var jdx = 0
        Projection.this.foreach(a => {
          val i = f(a)
          val length = i.asInstanceOf[Collection[B]].size
          if (idx < jdx + length) {
            val j = i.elements
            while (jdx < idx) {
              j.next; jdx += 1
            }
            return j.next
          } else jdx += length
        })
        throw new IndexOutOfBoundsException
      }
    }
    override def filter(p : A => Boolean) : Projection[A] = new Filter(p)
  }
}


/** Class <code>Seq[A]</code> represents finite sequences of elements
 *  of type <code>A</code>.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait Seq[+A] extends AnyRef with PartialFunction[Int, A] with Collection[A] {

  /** Returns the length of the sequence.
   *
   *  @return the sequence length.
   */
  def length: Int

  /** should always be <code>length</code> */
  def size = length

  /** Returns true if length == 0
   */
  override def isEmpty: Boolean = { length == 0 }

  /** Appends two iterable objects.
   *
   *  @return     the new iterable object
   *  @deprecated use <code>++</code> instead
   */
  @deprecated
  override def concat [B >: A](that: Iterable[B]): Seq[B] = {
    val buf = new ArrayBuffer[B]
    this copyToBuffer buf
    that copyToBuffer buf
    buf
  }

  /** Returns the last element of this list.
   *
   *  @return the last element of the list.
   *  @throws Predef.UnsupportedOperationException if the list is empty.
   */
  def last: A = length match {
  case 0 => throw new Predef.NoSuchElementException
  case n => this(n - 1)
  }

  /** Appends two iterable objects.
   */
  override def ++ [B >: A](that: Iterable[B]): Seq[B] = {
    val buf = new ArrayBuffer[B]
    this copyToBuffer buf
    that copyToBuffer buf
    buf
  }

  /** Is this partial function defined for the index <code>x</code>?
   *
   *  @param x ..
   *  @return  <code>true</code>, iff <code>x</code> is a legal sequence index.
   */
  def isDefinedAt(x: Int): Boolean = (x >= 0) && (x < length)

  /** Returns the index of the last occurence of the specified element
   *  in this sequence, or -1 if the sequence does not contain this element.
   *
   *  @param  elem   element to search for.
   *  @return the index in this sequence of the last occurence of the
   *          specified element, or -1 if the sequence does not contain
   *          this element.
   */
  def lastIndexOf[B >: A](elem: B): Int = {
    var i = length;
    var found = false;
    while (!found && (i > 0)) {
      i = i - 1;
      if (this(i) == elem) {
        found = true;
      }
    }
    if (found) i else -1;
  }

  /** Returns the sequence resulting from applying the given function
   *  <code>f</code> to each element of this sequence.
   *
   *  @param f function to apply to each element.
   *  @return  <code>f(a<sub>0</sub>), ..., f(a<sub>n</sub>)</code> if this
   *           sequence is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  override def map[B](f: A => B): Seq[B] = {
    // todo: malformed scala signature suing build when replaced by
    // super.map(f).asInstanceOf[Seq[B2]]
    val buf = new ArrayBuffer[B]
    val elems = elements
    while (elems.hasNext) buf += f(elems.next)
    buf
  }

  /** Applies the given function <code>f</code> to each element of
   *  this sequence, then concatenates the results.
   *
   *  @param f the function to apply on each element.
   *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
   *           this sequence is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  override def flatMap[B](f: A => Iterable[B]): Seq[B] = {
    val buf = new ArrayBuffer[B]
    val elems = elements
    while (elems.hasNext) f(elems.next) copyToBuffer buf
    buf
  }

  /** Returns all the elements of this sequence that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the list.
   *  @return the elements of this list satisfying <code>p</code>.
   */
  override def filter(p: A => Boolean): Seq[A] = super.filter(p).asInstanceOf[Seq[A]]

  /** Returns a sequence consisting only over the first <code>n</code>
   *  elements of this sequence, or else the whole sequence, if it has less
   *  than <code>n</code> elements.
   *
   *  @param n the number of elements to take
   *  @return  the new sequence
   */
  override def take(n: Int): Seq[A] = super.take(n).asInstanceOf[Seq[A]]

  /** Returns this sequence without its <code>n</code> first elements
   *  If this sequence has less than <code>n</code> elements, the empty
   *  sequence is returned.
   *
   *  @param n the number of elements to drop
   *  @return  the new sequence
   */
  override def drop(n: Int): Seq[A] = super.drop(n).asInstanceOf[Seq[A]]

  /** Returns the longest prefix of this sequence whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest prefix of this sequence whose elements satisfy
   *           the predicate <code>p</code>.
   */
  override def takeWhile(p: A => Boolean): Seq[A] = super.takeWhile(p).asInstanceOf[Seq[A]]

  /** Returns the longest suffix of this sequence whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest suffix of the sequence whose first element
   *           does not satisfy the predicate <code>p</code>.
   */
  override def dropWhile(p: A => Boolean): Seq[A] = super.dropWhile(p).asInstanceOf[Seq[A]]

  /** A sequence consisting of all elements of this sequence in reverse order.
   */
  def reverse: Seq[A] = {
    var result: List[A] = Nil
    val elems = elements
    while (elems.hasNext) result = elems.next :: result
    result
  }

  /** Tests if the given value <code>elem</code> is a member of this
   *  sequence.
   *
   *  @param elem element whose membership has to be tested.
   *  @return     <code>true</code> iff there is an element of this sequence
   *              which is equal (w.r.t. <code>==</code>) to <code>elem</code>.
   */
  def contains(elem: Any): Boolean = exists (.==(elem))

  /** Returns a subsequence starting from index <code>from</code>
   *  consisting of <code>len</code> elements.
   */
  def slice(from: Int, len: Int): Seq[A] = this.drop(from).take(len)

  /** Returns a subsequence starting from index <code>from</code>
   *  consisting of <code>len</code> elements.
   *
   *  @deprecated use <code>slice</code> instead
   */
  @deprecated
  def subseq(from: Int, end: Int): Seq[A] = slice(from, end - from)


  /** Converts this sequence to a fresh Array with <code>length</code> elements.
   */
  override def toArray[B >: A]: Array[B] = {
    val result = new Array[B](length)
    copyToArray(result, 0)
    result
  }
  override def projection : Seq.Projection[A] = new Seq.Projection[A] {
    def elements = Seq.this.elements
    def length = Seq.this.length
    def apply(idx : Int) = (Seq.this.apply(idx))
    override def stringPrefix = Seq.this.stringPrefix + "P"
  }

  class Filter(p : A => Boolean) extends Seq.Projection[A] {
    override def stringPrefix = Seq.this.stringPrefix + "F"
    override def elements = Seq.this.elements.filter(p)
    override def apply(idx : Int) : A = {
      var jdx = 0
      val i = Seq.this.elements
      while (true) {
        val a = i.next
        if (p(a)) {
          if (jdx == idx) return a
          else jdx = jdx + 1
        }
      }
      throw new Predef.Error
    }
    override def length = {
      var sz = 0
      val i = Seq.this.elements
      while (i.hasNext) if (p(i.next)) sz = sz + 1
      sz
    }
  }

}

