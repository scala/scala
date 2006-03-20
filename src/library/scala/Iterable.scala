/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;

import Predef.error;

object Iterable {
  implicit def view[A <% Ordered[A]](x: Iterable[A]): Ordered[Iterable[A]] =
    new Ordered[Iterable[A]] {
      def compare[B >: Iterable[A] <% Ordered[B]](that: B): Int = that match {
        case y: Iterable[A] =>
          val xs = x.elements;
	  val ys = y.elements;
	  var res = 0;
	  while (xs.hasNext && ys.hasNext && (res == 0)) {
            res = xs.next compare ys.next;
          }
	  if (xs.hasNext) 1
	  else if (ys.hasNext) -1
          else res;
        case _ =>
          -(that compare x)
      }
    }

  /** The minimum element of a non-empty sequence of ordered elements */
  def min[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.elements;
    if (!xs.hasNext) error("min(<empty>)");
    var min = xs.next;
    while (xs.hasNext) {
      val x = xs.next;
      if (x < min) min = x;
    }
    min
  }

  /** The maximum element of a non-empty sequence of ordered elements */
  def max[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.elements;
    if (!xs.hasNext) error("max(<empty>)");
    var max = xs.next;
    while (xs.hasNext) {
      val x = xs.next;
      if (max < x) max = x;
    }
    max
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
    def elements: Iterator[A];

    /** Concatenates two iterable objects
     *
     *  @return the new iterable object
     *  @author buraq
     */
    def concat[B >: A](that:Iterable[B]): Iterable[B] = new Iterable[B] {
      def elements: Iterator[B] = Iterable.this.elements.append(that.elements);
    }

    /** Apply a function <code>f</code> to all elements of this
     *  iterable object.
     *
     *  @param  f   a function that is applied to every element.
     */
    def foreach(f: A => Unit): Unit = elements.foreach(f);

    /** Apply a predicate <code>p</code> to all elements of this
     *  iterable object and return true, iff the predicate yields
     *  true for all elements.
     *
     *  @param   p     the predicate
     *  @returns true, iff the predicate yields true for all elements.
     */
    def forall(p: A => Boolean): Boolean = elements.forall(p);

    /** Apply a predicate <code>p</code> to all elements of this
     *  iterable object and return true, iff there is at least one
     *  element for which <code>p</code> yields true.
     *
     *  @param   p     the predicate
     *  @returns true, iff the predicate yields true for at least one element.
     */
    def exists(p: A => Boolean): Boolean = elements.exists(p);

    /** Find and return the first element of the iterable object satisfying a
     *  predicate, if any.
     *
     *  @param p the predicate
     *  @return the first element in the iterable object satisfying <code>p</code>,
     *  or <code>None</code> if none exists.
     */
    def find(p: A => Boolean): Option[A] = elements.find(p);

    /** Combines the elements of this list together using the binary
     *  operator <code>op</code>, from left to right, and starting with
     *  the value <code>z</code>.
     *  @return <code>op(... (op(op(z,a0),a1) ...), an)</code> if the list
     *  is <code>List(a0, a1, ..., an)</code>.
     */
    def foldLeft[B](z: B)(op: (B, A) => B): B = elements.foldLeft(z)(op);

    /** Combines the elements of this list together using the binary
     *  operator <code>op</code>, from rigth to left, and starting with
     *  the value <code>z</code>.
     *  @return <code>a0 op (... op (an op z)...)</code> if the list
     *  is <code>[a0, a1, ..., an]</code>.
     */
    def foldRight[B](z: B)(op: (A, B) => B): B = elements.foldRight(z)(op);

    /** Similar to <code>foldLeft</code> but can be used as
     *  an operator with the order of list and zero arguments reversed.
     *  That is, <code>z /: xs</code> is the same as <code>xs foldLeft z</code>
     */
    def /:[B](z: B)(f: (B, A) => B): B = foldLeft(z)(f);

    /** An alias for <code>foldRight</code>.
     *  That is, <code>xs :\ z</code> is the same as <code>xs foldRight z</code>
     */
    def :\[B](z: B)(f: (A, B) => B): B = foldRight(z)(f);

    /** Checks if the other iterable object contains the same elements.
     *
     *  @param that  the other iterable object
     *  @return true, iff both iterable objects contain the same elements.
     */
    def sameElements[B >: A](that: Iterable[B]): Boolean = {
        val ita = this.elements;
        val itb = that.elements;
        var res = true;
        while (res && ita.hasNext && itb.hasNext) {
            res = (ita.next == itb.next);
        }
        !ita.hasNext && !itb.hasNext && res
    }
}
