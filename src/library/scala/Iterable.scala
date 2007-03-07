/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


import Predef.IllegalArgumentException
import collection.mutable.{Buffer,ArrayBuffer}
import compat.StringBuilder

/** This object ...
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
}


/** Collection classes mixing in this class provide a method
 *  <code>elements</code> which returns an iterator over all the
 *  elements contained in the collection.
 *
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
   */
  @deprecated
  def concat[B >: A](that: Iterable[B]): Iterable[B] =
    this ++ that

  /** Appends two iterable objects.
   *
   *  @return the new iterable object
   */
  def ++ [B >: A](that: Iterable[B]): Iterable[B] = {
    val buf = new ArrayBuffer[B]
    this copyToBuffer buf
    that copyToBuffer buf
    buf
  }

  /** Returns the iterable resulting from applying the given function
   *  <code>f</code> to each element of this iterable.
   *
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
   *  @param p the predicate used to filter the list.
   *  @return the elements of this list satisfying <code>p</code>.
   */
  def filter(p: A => Boolean): Iterable[A] = {
    val buf = new ArrayBuffer[A]
    val elems = elements
    while (elems.hasNext) { val x = elems.next; if (p(x)) buf += x }
    buf
  }

  /** Returns the longest prefix of this iterable whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest prefix of this iterable whose elements satisfy
   *           the predicate <code>p</code>.
   */
  def takeWhile(p: A => Boolean): Iterable[A] =
    new ArrayBuffer[A] ++ elements.takeWhile(p)

  /** Returns the longest suffix of this iterable whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest suffix of the iterable whose first element
   *           does not satisfy the predicate <code>p</code>.
   */
  def dropWhile(p: A => Boolean): Iterable[A] =
    new ArrayBuffer[A] ++ elements.dropWhile(p)

  /** Returns an iterable consisting only over the first <code>n</code>
   *  elements of this iterable, or else the whole iterable, if it has less
   *  than <code>n</code> elements.
   *
   *  @param n the number of elements to take
   *  @return  the new iterable
   */
  def take(n: Int): Iterable[A] =
    new ArrayBuffer[A] ++ elements.take(n)

  /** Returns this iterable without its <code>n</code> first elements
   *  If this iterable has less than <code>n</code> elements, the empty
   *  iterable is returned.
   *
   *  @param n the number of elements to drop
   *  @return  the new iterable
   */
  def drop(n: Int): Iterable[A] =
    new ArrayBuffer[A] ++ elements.drop(n)

  /** Apply a function <code>f</code> to all elements of this
   *  iterable object.
   *
   *  @param  f   a function that is applied to every element.
   */
  def foreach(f: A => Unit): Unit = elements.foreach(f)

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return true, iff the predicate yields
   *  true for all elements.
   *
   *  @param   p     the predicate
   *  @return true, iff the predicate yields true for all elements.
   */
  def forall(p: A => Boolean): Boolean = elements.forall(p)

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return true, iff there is at least one
   *  element for which <code>p</code> yields true.
   *
   *  @param   p     the predicate
   *  @return true, iff the predicate yields true for at least one element.
   */
  def exists(p: A => Boolean): Boolean = elements.exists(p)

  /** Find and return the first element of the iterable object satisfying a
   *  predicate, if any.
   *
   *  @param p the predicate
   *  @return the first element in the iterable object satisfying <code>p</code>,
   *  or <code>None</code> if none exists.
   */
  def find(p: A => Boolean): Option[A] = elements.find(p)

  /** Returns index of the first element satisying a predicate, or -1.
   *
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
   *  @return <code>f(... (f(f(z, a<sub>0</sub>), a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the list is
   *          <code>[a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub>]</code>.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B = elements.foldLeft(z)(op)

  /** Combines the elements of this list together using the binary
   *  function <code>f</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>f(a<sub>0</sub>, f(a<sub>1</sub>, f(..., f(a<sub>n</sub>, z)...)))</code>
   *          if the list is <code>[a<sub>0</sub>, a1, ..., a<sub>n</sub>]</code>.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B = elements.foldRight(z)(op)

  /** Similar to <code>foldLeft</code> but can be used as
   *  an operator with the order of list and zero arguments reversed.
   *  That is, <code>z /: xs</code> is the same as <code>xs foldLeft z</code>
   */
  def /:[B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  /** An alias for <code>foldRight</code>.
   *  That is, <code>xs :\ z</code> is the same as <code>xs foldRight z</code>
   */
  def :\[B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from left to right
   *  @param op  The operator to apply
   *  @return <code>op(... op(a<sub>0</sub>,a<sub>1</sub>), ..., a<sub>n</sub>)</code>
      if the iterable object has elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   *  @throws Predef.UnsupportedOperationException if the iterable object is empty.
   */
  def reduceLeft[B >: A](op: (B, B) => B): B = elements.reduceLeft(op)

/** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from right to left
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
   *  @param  dest   The buffer to which elements are copied
   */
  def copyToBuffer[B >: A](dest: Buffer[B]): Unit = elements copyToBuffer dest

  /** Checks if the other iterable object contains the same elements.
   *
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
   * @return a list with all the elements of this iterable object
   */
  def toList: List[A] = elements.toList

  /** Returns a string representation of this iterable object. The resulting string
   *  begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>.
   *  <p/>
   *  Ex: <br/>
   *  <code>List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"</code>
   *
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
   *  @param sep separator string.
   *  @return a string representation of this iterable object.
   */
  def mkString(sep: String): String = this.mkString("", sep, "")

  /** Write all elements of this string into given string builder.
   *
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
}
