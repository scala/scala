/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

/** This class represents mutable sets. Concrete set implementations
 *  just have to provide functionality for the abstract methods in
 *  <code>scala.collection.Set</code> as well as for <code>add</code>,
 *  <code>remove</code>, and <code>clear</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 09/05/2004
 */
[cloneable]
trait Set[A] extends AnyRef with collection.Set[A]
      with Scriptable[Message[A]]
{

  /** This method allows one to add or remove an element <code>elem</code>
   *  from this set depending on the value of parameter <code>included</code>.
   *  Typically, one would use the following syntax:
   *  <pre>set(elem) = true</pre>
   *
   *  @param elem     ...
   *  @param included ...
   */
  def update(elem: A, included: Boolean): Unit =
    if (included) +=(elem) else -=(elem)

  /** This method adds a new element to the set.
   *
   *  @param elem ...
   */
  def +=(elem: A): Unit

  /** This method will add all the elements provided by an iterator
   *  of the iterable object <code>that</code> to the set.
   *
   *  @param that ...
   */
  def ++=(that: Iterable[A]): Unit = ++=(that.elements);

  /** This method will add all the elements provided by an iterator
   *  of the iterable object <code>that</code> to the set.
   *
   *  @param it ...
   */
  def ++=(it: Iterator[A]): Unit = it foreach +=

  /** <code>incl</code> can be used to add many elements to the set
   *  at the same time.
   *
   *  @param elems ...
   */
  def incl(elems: A*): Unit = ++=(elems.elements)

  /** <code>-=</code> can be used to remove a single element from
   *  a set.
   */
  def -=(elem: A): Unit

  /** This method removes all the elements provided by the
   *  the iterable object <code>that</code> from the set.
   */
  def --=(that: Iterable[A]): Unit = --=(that.elements)

  /** This method removes all the elements provided by an iterator
   *  <code>it</code> from the set.
   */
  def --=(it: Iterator[A]): Unit = it foreach -=

  /** <code>excl</code> removes many elements from the set.
   */
  def excl(elems: A*): Unit = --=(elems.elements)

  /** This method computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that ...
   */
  def intersect(that: Set[A]): Unit = filter(that.contains)

  /** Method <code>filter</code> removes all elements from the set for
   *  which the predicate <code>p</code> yields the value <code>false</code>.
   */
  def filter(p: A => Boolean): Unit = toList.foreach {
    elem => if (!p(elem)) -=(elem)
  }

  /** Removes all elements from the set. After this operation is completed,
   *  the set will be empty.
   */
  def clear: Unit

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   */
  def <<(cmd: Message[A]): Unit = cmd match {
    case Include(elem) => this += elem
    case Remove(elem) => this -= elem
    case Reset() => clear
    case s: Script[A] => s.elements foreach <<
    case _ => error("message " + cmd + " not understood")
  }

  /** Return a clone of this set.
   *
   *  @return  a set with the same elements.
   */
  override def clone(): Set[A] = super.clone().asInstanceOf[Set[A]]

  /** The hashCode method always yields an error, since it is not
   *  safe to use mutable stacks as keys in hash tables.
   *
   *  @return never.
   */
  override def hashCode(): Int = error("unsuitable as hash key")
}
