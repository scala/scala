/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


/** This class implements a proxy for iterable objects. It forwards
 *  all calls to a different iterable object.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 26/04/2004
 */
trait IterableProxy[+A] extends Iterable[A] with Proxy {

  def self: Iterable[A]

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def elements: Iterator[A] = self.elements

  /** Apply a function <code>f</code> to all elements of this
   *  iterable object.
   *
   *  @param  f   a function that is applied to every element.
   */
  override def foreach(f: A => Unit): Unit = self.foreach(f)

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return true, iff the predicate yields
   *  true for all elements.
   *
   *  @param   p     the predicate
   *  @returns true, iff the predicate yields true for all elements.
   */
  override def forall(p: A => Boolean): Boolean = self.forall(p)

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return true, iff there is at least one
   *  element for which <code>p</code> yields true.
   *
   *  @param   p     the predicate
   *  @returns true, iff the predicate yields true for at least one element.
   */
  override def exists(p: A => Boolean): Boolean = self.exists(p)

  /** Find and return the first element of the iterable object satisfying a
   *  predicate, if any.
   *
   *  @param p the predicate
   *  @return the first element in the iterable object satisfying <code>p</code>,
   *  or <code>None</code> if none exists.
   */
  override def find(p: A => Boolean): Option[A] = self.find(p)

  /** Combines the elements of this list together using the binary
   *  operator <code>op</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *  @return <code>op(... (op(op(z,a0),a1) ...), an)</code> if the list
   *  is <code>List(a0, a1, ..., an)</code>.
   */
  override def foldLeft[B](z: B)(op: (B, A) => B): B = self.foldLeft(z)(op)

  /** Combines the elements of this list together using the binary
   *  operator <code>op</code>, from rigth to left, and starting with
   *  the value <code>z</code>.
   *  @return <code>a0 op (... op (an op z)...)</code> if the list
   *  is <code>[a0, a1, ..., an]</code>.
   */
  override def foldRight[B](z: B)(op: (A, B) => B): B = self.foldRight(z)(op)

  /** Similar to <code>foldLeft</code> but can be used as
   *  an operator with the order of list and zero arguments reversed.
   *  That is, <code>z /: xs</code> is the same as <code>xs foldLeft z</code>
   */
  override def /:[B](z: B)(f: (B, A) => B): B = self./:(z)(f)

  /** An alias for <code>foldRight</code>.
   *  That is, <code>xs :\ z</code> is the same as <code>xs foldRight z</code>
   *
   *  @param z ...
   *  @param f ...
   *  @return ...
   */
  override def :\[B](z: B)(f: (A, B) => B): B = self.:\(z)(f)

  /** Checks if the other iterable object contains the same elements.
   *
   *  @param that  the other iterable object
   *  @return true, iff both iterable objects contain the same elements.
   */
  override def sameElements[B >: A](that: Iterable[B]): Boolean =
    self.sameElements(that)

  override def toList: List[A] = self.toList
}
