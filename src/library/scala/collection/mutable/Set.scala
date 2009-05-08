/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import generic._

/** This class represents mutable sets. Concrete set implementations
 *  just have to provide functionality for the abstract methods in
 *  <a href="../Set.html" target="contentFrame">
 *  <code>scala.collection.Set</code></a> as well as for <code>+=</code>,
 *  <code>-= and <code>clear</code>.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 */
trait Set[A] extends Iterable[A]
                with collection.Set[A]
                with SetTemplate[A, Set[A]]
                with Growable[A]
                with Shrinkable[A]
                with Cloneable[Set[A]]
                with Unhashable {
self =>

  override def empty = Set.empty

  override def traversableBuilder[B]: Builder[B, Set[B], Any] = Set.newBuilder[B]

  /** This method allows one to add or remove an element <code>elem</code>
   *  from this set depending on the value of parameter <code>included</code>.
   *  Typically, one would use the following syntax:
   *  <pre>set(elem) = true</pre>
   *
   */
  def update(elem: A, included: Boolean) {
    if (included) this += elem else this -= elem
  }

  /** Adds a new element to the set.
   *
   *  @param elem the element to be added
   */
  def +=(elem: A)

  /** Removes a single element from a set.
   *  @param elem  The element to be removed.
   */
  def -=(elem: A)

  /** Adds a new element to the set and returns the set itself.
   *
   *  @param elem the element to be added
   */
  override def +(elem: A): this.type = { +=(elem); this }

  /** Removed a new element from the set and returns the set itself.
   *
   *  @param elem the element to be added
   */
  override def -(elem: A): this.type = { -=(elem); this }

  /** This method computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that the set to intersect with.
   */
  override def intersect(that: collection.Set[A]): this.type = { retain(that.contains); this }

  /** Method <code>retain removes all elements from the set for
   *  which the predicate <code>p</code> yields the value <code>false</code>.
   */
  def retain(p: A => Boolean): Unit = for (elem <- this.toList) if (!p(elem)) this -= elem

  /** Removes all elements from the set. After this operation is completed,
   *  the set will be empty.
   */
  def clear() { foreach(-=) }

  override def clone(): Set[A] = { val b = newBuilder; b ++= this; b.result }

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   *  @throws <code>Predef.UnsupportedOperationException</code>
   *  if the message was not understood.
   def <<(cmd: Message[A]): Unit = cmd match {
    case Include(elem) => this += elem
    case Remove(elem) => this -= elem
    case Reset() => clear
    case s: Script[_] => s.elements foreach <<
    case _ => throw new UnsupportedOperationException("message " + cmd + " not understood")
  }
  */
}

/** The canonical factory methods for <a href="Set.html">mutable sets</a>.
 *  Currently this returns a HashSet.
 */
object Set extends SetFactory[Set] {
  type Coll = Set[_]
  implicit def builderFactory[A]: BuilderFactory[A, Set[A], Coll] = new BuilderFactory[A, Set[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def empty[A]: Set[A] = HashSet.empty[A]
}

