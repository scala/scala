/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import scala.runtime.compat.StringBuilder

object Seq {

  /** builds a singleton sequence
   *  @author buraq
   */
  def single[A](x: A) = new Seq[A] {
    def length = 1
    def elements = Iterator.single(x)
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
}


/** Class <code>Seq[A]</code> represents finite sequences of elements
 *  of type <code>A</code>.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait Seq[+A] extends AnyRef with PartialFunction[Int, A] with Iterable[A] {

  /** Returns the length of the sequence.
   *
   *  @return the sequence length.
   */
  def length: Int

  /** Returns the concatenation of two sequences.
   *
   *  @return concatenation of this sequence with argument
   *  @author buraq
   */
  def concat[B >: A](that: Seq[B]): Seq[B] = new Seq[B] {
    def length = Seq.this.length + that.length
    def elements: Iterator[B] = Seq.this.elements.append(that.elements)
    def apply(i: Int) =
      if (Seq.this.isDefinedAt(i)) Seq.this.apply(i)
      else that.apply(i - Seq.this.length)
  }

  /** Is this partial function defined for the index <code>x</code>?
   *
   *  @return true, iff <code>x</code> is a legal sequence index.
   */
  def isDefinedAt(x: Int): Boolean = (x >= 0) && (x < length)

  /** Returns the index of the first occurence of the specified
   *  object in this sequence.
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

  /** Returns the sub-sequence starting from index <code>n</code>.
   *
   *  @param n ...
   */
  def take(n: Int): Seq[A] = subseq(0, n)

  /** Returns a new sub-sequence that drops the first <code>n</code>
   *  elements of this sequence.
   *
   *  @param n ...
   */
  def drop(n: Int): Seq[A] = subseq(n, length - n)

  /** Returns a subsequence starting from index <code>from</code>
  *  consisting of <code>len</code> elements.
  */
  def subseq(from: Int, len: Int): Seq[A] =
    if ((from + len) <= length) new Seq[A] {
      def apply(n: Int): A = Seq.this.apply(n + from);
      def length: Int = len;
      def elements: Iterator[A] = new Iterator[A] {
        var i = from;
        def hasNext = (i < (from + len));
        def next = {
          val res = Seq.this.apply(i);
          i = i + 1;
          res
        }
      }
    } else
      Predef.error("cannot create subsequence");

  /** Transform this sequence into a list of all elements.
   *
   *  @return  a list which enumerates all elements of this sequence.
   */
  def toList: List[A] = elements.toList

  /** Converts this sequence to a fresh Array */
  def toArray[B >: A]: Array[B] =
    elements.copyToArray(new Array[B](length), 0)

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @return the given array <code>xs</code> filled with this list.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int): Array[B] =
    elements.copyToArray(xs, start)

  /** Customizes the <code>toString</code> method.
   *
   *  @return a string representation of this sequence.
   */
  override def toString() = mkString(stringPrefix+"(", ",", ")")

  /** Returns a string representation of this sequence. The resulting string
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
   *  @return a string representation of this sequence.
   */
  def mkString(start: String, sep: String, end: String): String = {
    val buf = new StringBuilder()
    buf.append(start)
    val elems = elements
    if (elems.hasNext) buf.append(elems.next)
    while (elems.hasNext) {
      buf.append(sep); buf.append(elems.next)
    }
    buf.append(end)
    buf.toString
  }

  /** Defines the prefix of the string representation.
   */
  protected def stringPrefix: String = "Seq"
}

