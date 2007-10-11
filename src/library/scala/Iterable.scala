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
import collection.mutable.{Buffer,ArrayBuffer}
import compat.StringBuilder

/** Various utilities for instances of <a href="Iterable.html">Iterable</a>.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 04/02/2004
 */
object Iterable {
/*
  implicit def view[A <% Ordered[A]](x: Iterable[A]): Ordered[Iterable[A]] =
    new Ordered[Iterable[A]] {
      def compare[B >: Iterable[A] <% Ordered[B]](that: B): Int = that match {
        case y: Iterable[A] =>
          val xs = x.elements
          val ys = y.elements
          var res = 0
          while (xs.hasNext && ys.hasNext && (res == 0)) {
            res = xs.next compare ys.next
          }
          if (xs.hasNext) 1
          else if (ys.hasNext) -1
          else res
        case _ =>
          -(that compare x)
      }
    }
*/
  /** The minimum element of a non-empty sequence of ordered elements */
  def min[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.elements
    if (!xs.hasNext) throw new IllegalArgumentException("min(<empty>)")
    var min = xs.next
    while (xs.hasNext) {
      val x = xs.next
      if (x < min) min = x
    }
    min
  }

  /** The maximum element of a non-empty sequence of ordered elements */
  def max[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.elements
    if (!xs.hasNext) throw new IllegalArgumentException("max(<empty>)")
    var max = xs.next
    while (xs.hasNext) {
      val x = xs.next
      if (max < x) max = x
    }
    max
  }

  /** The empty iterable object */
  val empty = new Iterable[Nothing] {
    def elements = Iterator.empty
  }
  /** A non-strict projection of an iterable.
   * @author Sean McDirmid
   */
  trait Projection[+A] extends Iterable[A] {
    override def projection = this
    /** convert to a copied strict collection */
    def force : Iterable[A] = toList

    /** non-strict */
    override def filter(p : A => Boolean) : Projection[A] = new Projection[A] {
      def elements = Projection.this.elements.filter(p)
    }
    /** non-strict */
    override def map[B](f: A => B) : Projection[B] = new Projection[B] {
      def elements = Projection.this.elements.map(f)
    }
    /** non-strict */
    override def flatMap[B](f: A => Iterable[B]) : Projection[B] = new Projection[B] {
      def elements = Projection.this.elements.flatMap(a => f(a).elements)
    }
    /** non-strict */
    override def takeWhile(p: A => Boolean): Projection[A] = new Projection[A] {
      def elements = Projection.this.elements.takeWhile(p)
    }
    /** The projection resulting from the concatenation of this projection with the <code>rest</code> projection.
     *  @param rest   The projection that gets appended to this projection
     */
    def append[B >: A](rest : => Iterable[B]): Projection[B] = new Projection[B] {
      def elements = Projection.this.elements ++ rest.elements
    }
  }
}


/** Collection classes mixing in this class provide a method
 *  <code>elements</code> which returns an iterator over all the
 *  elements contained in the collection.
 *
 *  @note If a collection has a known <code>size</code>, it should also sub-type <code>Collection</code>.
 *        Only potentially unbounded collections should directly sub-class <code>Iterable</code>.
 *  @author  Matthias Zenger
 *  @version 1.1, 04/02/2004
 */
trait Iterable[+A] {

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def elements: Iterator[A]

  /** Appends two iterable objects.
   *
   *  @return the new iterable object
   *  @deprecated  use <code>++</code> instead
   *  @note Will not terminate for infinite-sized collections.
   */
  @deprecated
  def concat[B >: A](that: Iterable[B]): Collection[B] =
    this ++ that

  /** Appends two iterable objects.
   *
   *  @return the new iterable object
   *  @note Will not terminate for infinite-sized collections.
   */
  def ++ [B >: A](that: Iterable[B]): Collection[B] = {
    val buf = new ArrayBuffer[B]
    this copyToBuffer buf
    that copyToBuffer buf
    buf
  }

  /** Returns the iterable resulting from applying the given function
   *  <code>f</code> to each element of this iterable.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param f function to apply to each element.
   *  @return <code>f(a<sub>0</sub>), ..., f(a<sub>n</sub>)</code>
   *          if this iterable is <code>a<sub>0</sub>, ..., an</code>.
   */
  def map[B](f: A => B): Iterable[B] = {
    val buf = new ArrayBuffer[B]
    val elems = elements
    while (elems.hasNext) buf += f(elems.next)
    buf
  }

  /** Applies the given function <code>f</code> to each element of
   *  this iterable, then concatenates the results.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param f the function to apply on each element.
   *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
   *           this iterable is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  def flatMap[B](f: A => Iterable[B]): Iterable[B] = {
    val buf = new ArrayBuffer[B]
    val elems = elements
    while (elems.hasNext) f(elems.next) copyToBuffer buf
    buf
  }

  /** Returns all the elements of this iterable that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param p the predicate used to filter the list.
   *  @return the elements of this list satisfying <code>p</code>.
   */
  def filter(p: A => Boolean): Iterable[A] = {
    val buf = new ArrayBuffer[A]
    val elems = elements
    while (elems.hasNext) { val x = elems.next; if (p(x)) buf += x }
    buf
  }

  /** Partitions this iterable in two iterables according to a predicate.
   *
   *  @param p the predicate on which to partition
   *  @return  a pair of iterables: the iterable that satisfy the predicate
   *           <code>p</code> and the iterable that do not.
   *           The relative order of the elements in the resulting iterables
   *           is the same as in the original iterable.
   */
  def partition(p: A => Boolean): (Iterable[A], Iterable[A]) = {
    val matched = new ArrayBuffer[A]
    val failed = new ArrayBuffer[A]
    val elems = elements
    while (elems.hasNext) { val x = elems.next; if (p(x)) matched += x else failed += x }
    (matched, failed)
  }

  /** Returns the longest prefix of this iterable whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @note May not terminate for infinite-sized collections.
   *  @param p the test predicate.
   *  @return  the longest prefix of this iterable whose elements satisfy
   *           the predicate <code>p</code>.
   */
  def takeWhile(p: A => Boolean): Iterable[A] =
    (new ArrayBuffer[A] ++ elements.takeWhile(p))

  /** Returns the longest suffix of this iterable whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @note May not terminate for infinite-sized collections.
   *  @param p the test predicate.
   *  @return  the longest suffix of the iterable whose first element
   *           does not satisfy the predicate <code>p</code>.
   */
  def dropWhile(p: A => Boolean): Collection[A] =
    (new ArrayBuffer[A] ++ elements.dropWhile(p))

  /** Returns an iterable consisting only over the first <code>n</code>
   *  elements of this iterable, or else the whole iterable, if it has less
   *  than <code>n</code> elements.
   *
   *  @deprecated API does not make sense for non-ordered collections
   *  @param n the number of elements to take
   *  @return  the new iterable
   */
  @deprecated def take(n: Int): Collection[A] =
    (new ArrayBuffer[A] ++ elements.take(n))

  /** Returns this iterable without its <code>n</code> first elements
   *  If this iterable has less than <code>n</code> elements, the empty
   *  iterable is returned.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @deprecated API does not make sense for non-ordered collections
   *  @param n the number of elements to drop
   *  @return  the new iterable
   */
  @deprecated def drop(n: Int): Collection[A] =
    (new ArrayBuffer[A] ++ elements.drop(n))

  /** Apply a function <code>f</code> to all elements of this
   *  iterable object.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  f   a function that is applied to every element.
   */
  def foreach(f: A => Unit): Unit = elements.foreach(f)

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return true, iff the predicate yields
   *  true for all elements.
   *
   *  @note May not terminate for infinite-sized collections.
   *  @param   p     the predicate
   *  @return true, iff the predicate yields true for all elements.
   */
  def forall(p: A => Boolean): Boolean = elements.forall(p)

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return true, iff there is at least one
   *  element for which <code>p</code> yields true.
   *
   *  @note May not terminate for infinite-sized collections.
   *  @param   p     the predicate
   *  @return true, iff the predicate yields true for at least one element.
   */
  def exists(p: A => Boolean): Boolean = elements.exists(p)

  /** Find and return the first element of the iterable object satisfying a
   *  predicate, if any.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param p the predicate
   *  @return the first element in the iterable object satisfying <code>p</code>,
   *  or <code>None</code> if none exists.
   */
  def find(p: A => Boolean): Option[A] = elements.find(p)

  /** Returns index of the first element satisying a predicate, or -1.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   *  @return   the index of the first element satisfying <code>p</code>,
   *            or -1 if such an element does not exist
   */
  def findIndexOf(p: A => Boolean): Int = {
    val it = elements
    var i = 0
    while (it.hasNext)
      if (p(it.next))
        return i
      else
        i = i + 1
    return -1
  }

  /** Returns the index of the first occurence of the specified
   *  object in this iterable object.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  elem  element to search for.
   *  @return the index in this sequence of the first occurence of the
   *          specified element, or -1 if the sequence does not contain
   *          this element.
   */
  def indexOf[B >: A](elem: B): Int = {
    val it = elements
    var i = 0
    var found = false
    while (!found && it.hasNext) {
      if (it.next == elem) {
        found = true
      } else {
        i = i + 1
      }
    }
    if (found) i else -1
  }

  /** Combines the elements of this iterable object together using the binary
   *  function <code>f</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @return <code>f(... (f(f(z, a<sub>0</sub>), a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the list is
   *          <code>[a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub>]</code>.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B = elements.foldLeft(z)(op)

  /** Combines the elements of this list together using the binary
   *  function <code>f</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @return <code>f(a<sub>0</sub>, f(a<sub>1</sub>, f(..., f(a<sub>n</sub>, z)...)))</code>
   *          if the list is <code>[a<sub>0</sub>, a1, ..., a<sub>n</sub>]</code>.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B = elements.foldRight(z)(op)

  /** Similar to <code>foldLeft</code> but can be used as
   *  an operator with the order of list and zero arguments reversed.
   *  That is, <code>z /: xs</code> is the same as <code>xs foldLeft z</code>
   *  @note Will not terminate for infinite-sized collections.
   */
  def /:[B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  /** An alias for <code>foldRight</code>.
   *  That is, <code>xs :\ z</code> is the same as <code>xs foldRight z</code>
   *  @note Will not terminate for infinite-sized collections.
   */
  def :\[B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from left to right
   *  @note Will not terminate for infinite-sized collections.
   *  @param op  The operator to apply
   *  @return <code>op(... op(a<sub>0</sub>,a<sub>1</sub>), ..., a<sub>n</sub>)</code>
      if the iterable object has elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   *  @throws Predef.UnsupportedOperationException if the iterable object is empty.
   */
  def reduceLeft[B >: A](op: (B, B) => B): B = elements.reduceLeft(op)

/** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from right to left
   *  @note Will not terminate for infinite-sized collections.
   *  @param op  The operator to apply
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n-1</sub> op a<sub>n</sub>)...)</code>
   *          if the iterable object has elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.
   *
   *  @throws Predef.UnsupportedOperationException if the iterator is empty.
   */
  def reduceRight[B >: A](op: (B, B) => B): B = elements.reduceRight(op)

  /** Copy all elements to a given buffer
   *  @note Will not terminate for infinite-sized collections.
   *  @param  dest   The buffer to which elements are copied
   *  @note Will not terminate if not finite.
   */
  def copyToBuffer[B >: A](dest: Buffer[B]): Unit = elements copyToBuffer dest

  /** Checks if the other iterable object contains the same elements.
   *
   *  @note will not terminate for infinite-sized collections.
   *  @param that  the other iterable object
   *  @return true, iff both iterable objects contain the same elements.
   */
  def sameElements[B >: A](that: Iterable[B]): Boolean = {
    val ita = this.elements
    val itb = that.elements
    var res = true
    while (res && ita.hasNext && itb.hasNext) {
      res = (ita.next == itb.next)
    }
    !ita.hasNext && !itb.hasNext && res
  }

  /**
   *  Create a fresh list with all the elements of this iterable object.
   *  @note Will not terminate for infinite-sized collections.
   */
  def toList: List[A] = elements.toList

  /**
   *  Create a stream which contains all the elements of this iterable object.
   *  @note consider using <code>projection</code> for lazy behavior.
   */
  def toStream: Stream[A] = Stream.fromIterator(elements)

  /** Returns a string representation of this iterable object. The resulting string
   *  begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>.
   *
   *  @ex  <code>List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"</code>
   *  @note Will not terminate for infinite-sized collections.
   *  @param start starting string.
   *  @param sep separator string.
   *  @param end ending string.
   *  @return a string representation of this iterable object.
   */
  def mkString(start: String, sep: String, end: String): String = {
    val buf = new StringBuilder()
    addString(buf, start, sep, end).toString
  }

  /** Returns a string representation of this iterable object. The string
   *  representations of elements (w.r.t. the method <code>toString()</code>)
   *  are separated by the string <code>sep</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param sep separator string.
   *  @return a string representation of this iterable object.
   */
  def mkString(sep: String): String = this.mkString("", sep, "")

  /** Converts a collection into a flat <code>String</code> by each element's toString method.
   *  @note Will not terminate for infinite-sized collections.
   */
  def mkString = {
    val buf = new StringBuilder
    foreach(buf append _.toString)
    buf.toString
  }



  /** Write all elements of this string into given string builder.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param buf ...
   *  @return    ...
   */
  def addString(buf: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    buf.append(start)
    val elems = elements
    if (elems.hasNext) buf.append(elems.next)
    while (elems.hasNext) {
      buf.append(sep); buf.append(elems.next)
    }
    buf.append(end)
  }

  def addString(buf: StringBuilder, sep: String): StringBuilder = addString(buf, "", sep, "")

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @pre    the array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit =
    elements.copyToArray(xs, start)


  /** Is this collection empty? */
  def isEmpty = !elements.hasNext

  /**
   * returns a projection that can be used to call non-strict <code>filter</code>,
   * <code>map</code>, and <code>flatMap</code> methods that build projections
   * of the collection.
   */
  def projection : Iterable.Projection[A] = new Iterable.Projection[A] {
    def elements = Iterable.this.elements
    override def force = Iterable.this
  }

  /** returns true iff this collection has a bound size.
   *  Many APIs in this trait will not work on collections of
   *  unbound sizes.
   */
  def hasDefiniteSize = true

}
