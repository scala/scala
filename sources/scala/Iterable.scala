/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


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
    def foreach(f: A => Unit): Unit = elements foreach f;

    /** Apply a predicate <code>p</code> to all elements of this
     *  iterable object and return true, iff the predicate yields
     *  true for all elements.
     *
     *  @param   p     the predicate
     *  @returns true, iff the predicate yields true for all elements.
     */
	def forall(p: A => Boolean): Boolean = elements forall p;

	/** Apply a predicate <code>p</code> to all elements of this
	 *  iterable object and return true, iff there is at least one
	 *  element for which <code>p</code> yields true.
     *
     *  @param   p     the predicate
     *  @returns true, iff the predicate yields true for at least one element.
     */
	def exists(p: A => Boolean): Boolean = elements exists p;

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
	def foldRight[B](z: B)(f: (A, B) => B): B = match {
	  case Nil => z
	  case x :: xs => f(x, xs.foldRight(z)(f))
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

    /** Transform this iterable object into a list of all elements.
     *
     *  @return  a list which enumerates all elements of this set.
     */
    def toList: List[A] = {
        var res: List[A] = Nil;
        elements foreach { elem => res = elem :: res; }
        res.reverse
    }
}
