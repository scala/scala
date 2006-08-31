/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable


/** This class represents immutable sets. Concrete set implementations
 *  just have to provide functionality for the abstract methods in
 *  <code>scala.collection.Set</code> as well as for <code>+</code> and
 *  <code>-</code>.
 *
 * Note that abstract immutable.Set's are not covariant in their type
 * parameter.  This is because some subclasses cannot support the
 * <code>+</code> method for arbitrary types.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 03/05/2004
 */
trait Set[A] extends AnyRef with collection.Set[A] {

  /** This method creates a new set with an additional element.
   */
  def +(elem: A): Set[A]

  /** @return an empty set of the same type as this set
   */
  def empty: Set[A]

  /** <code>incl</code> can be used to add many elements to the set
   *  at the same time.
   */
  def incl(elems: A*): Set[A] = incl(elems)

  /** This method will add all the elements provided by an iterator
   *  of the iterable object <code>that</code> to the set.
   *
   *  @param that ...
   */
  def incl(that: Iterable[A]): Set[A] = {
    var res = this
    that.elements.foreach(elem => res = res + elem)
    res
  }

  /** <code>-</code> can be used to remove a single element from
   *  a set.
   */
  def -(elem: A): Set[A]

  /** <code>excl</code> removes many elements from the set.
   */
  def excl(elems: A*): Set[A] = excl(elems)

  /** This method removes all the elements provided by an iterator
   *  of the iterable object <code>that</code> from the set.
   */
  def excl(that: Iterable[A]): Set[A] = {
    var res = this
    that.elements.foreach(elem => res = res - elem)
    res
  }

  /** This method computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that ...
   */
  def intersect(that: scala.collection.Set[A]): Set[A] = filter(that.contains)

  /** Method <code>filter</code> removes all elements from the set for
   *  which the predicate <code>p</code> yields the value <code>false</code>.
   *
   *  @param p ...
   */
  def filter(p: A => Boolean): Set[A] = {
    var res = this
    toList foreach {
      elem => if (!p(elem)) res = res - elem
    }
    res
  }

  /** hashcode for this set */
  override def hashCode() =
    elements.foldLeft(0)((hash: Int, e: A) => hash + e.hashCode())

}

