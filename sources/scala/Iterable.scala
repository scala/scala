/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


object Iterable {
  def view[A <% Ordered[A]](x: Iterable[A]): Ordered[Iterable[A]] = new Ordered[Iterable[A]] {
    def compareTo[B >: Iterable[A] <% Ordered[B]](that: B): Int = that match {
      case y: Iterable[A] =>
        val xs = x.elements;
	val ys = y.elements;
	var res = 0;
	while (xs.hasNext && ys.hasNext && (res == 0)) {
          res = xs.next compareTo ys.next;
        }
	if (xs.hasNext) 1
	else if (ys.hasNext) -1
        else res;
      case _ =>
        -(that compareTo x)
    }
  }

  /** The minimum element of a non-empty sequence of ordered elements */
  def min[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.elements;
    if (!xs.hasNext) throw new Error("min(<empty>)");
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
    if (!xs.hasNext) throw new Error("max(<empty>)");
    var max = xs.next;
    while (xs.hasNext) {
      val x = xs.next;
      if (max < x) max = x;
    }
    max
  }
}

/** Collection classes supporting this trait provide a method
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

    /** Apply a function <code>f</code> to all elements of this
     *  iterable object.
     *
     *  @param  f   a function that is applied to every element.
     */
    def foreach(f: A => Unit): Unit = {
        val it = elements;
        while (it.hasNext) { f(it.next) }
    }

    /** Apply a predicate <code>p</code> to all elements of this
     *  iterable object and return true, iff the predicate yields
     *  true for all elements.
     *
     *  @param   p     the predicate
     *  @returns true, iff the predicate yields true for all elements.
     */
    def forall(p: A => Boolean): Boolean = {
        val it = elements;
        var res = true;
        while (res && it.hasNext) { res = p(it.next) }
        res
    }

    /** Apply a predicate <code>p</code> to all elements of this
     *  iterable object and return true, iff there is at least one
     *  element for which <code>p</code> yields true.
     *
     *  @param   p     the predicate
     *  @returns true, iff the predicate yields true for at least one element.
     */
    def exists(p: A => Boolean): Boolean = {
        val it = elements;
        var res = false;
        while (!res && it.hasNext) { res = p(it.next) }
        res
    }

    /** Find and return the first element of the iterable object satisfying a
     *  predicate, if any.
     *
     *  @param p the predicate
     *  @return the first element in the iterable object satisfying <code>p</code>,
     *  or <code>None</code> if none exists.
     */
    def find(p: A => Boolean): Option[A] = {
        val it = elements;
        var res: Option[A] = None;
        while (res.isEmpty && it.hasNext) {
          val e = it.next;
          if (p(e))
            res = Some(e);
        }
        res
    }

    /** Combines the elements of this list together using the binary
     *  operator <code>op</code>, from left to right, and starting with
     *  the value <code>z</code>.
     *  @return <code>op(... (op(op(z,a0),a1) ...), an)</code> if the list
     *  is <code>List(a0, a1, ..., an)</code>.
     */
    def foldLeft[B](z: B)(op: (B, A) => B): B = {
        val it = elements;
        var acc = z;
        while (it.hasNext) {
          acc = op(acc, it.next)
        }
        acc
    }

    /** Combines the elements of this list together using the binary
     *  operator <code>op</code>, from rigth to left, and starting with
     *  the value <code>z</code>.
     *  @return <code>a0 op (... op (an op z)...)</code> if the list
     *  is <code>[a0, a1, ..., an]</code>.
     */
    def foldRight[B](z: B)(op: (A, B) => B): B = {
        val it = elements;
        def fold(z: B): B =
            if (it.hasNext) op(it.next, fold(z)) else z;
        fold(z)
    }

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
