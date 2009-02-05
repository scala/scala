/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Set.scala 16893 2009-01-13 13:09:22Z cunei $


package scalax.collection.mutable

import collection.generic._
import collection.generic.mutable.{Growable, Shrinkable}

/** The canonical factory methods for <a href="Set.html">mutable sets</a>.
 *  Currently these return <a href="HashSet.html">HashSet's</a>.
 */
object Set extends generic.SetFactory[Set] {
  /** The empty map of this type; this is implemented as a hashtable */
  def empty[A]: Set[A] = null // !!! new HashSet[A]
}

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
@cloneable
trait Set[A] extends OrderedIterable[A]
                with collection.Set[A]
                with SetTemplate[Set, A]
                with Growable[A]
                with Shrinkable[A]
                with Cloneable[A] {
self =>

  /** This method allows one to add or remove an element <code>elem</code>
   *  from this set depending on the value of parameter <code>included</code>.
   *  Typically, one would use the following syntax:
   *  <pre>set(elem) = true</pre>
   *
   */
  def update(elem: A, included: Boolean) {
    if (included) this += elem else this -= elem
  }

  /** Add a new element to the set.
   *
   *  @param elem the element to be added
   */
  def +=(elem: A)

  /** Removes a single element from a set.
   *  @param elem  The element to be removed.
   */
  def -=(elem: A)

  // Bridge methods to methods in Growable/Shrinkable;
  // we can't directly inherit these because they override nothing.

  override def - (elem: A): this.type = super.-(elem)
  override def - (elem1: A, elem2: A, elems: A*): this.type = super.-(elem1, elem2, elems: _*)
  override def -- (elems: collection.Iterable[A]): this.type = super.--(elems)
  override def -- (elems: Iterator[A]): this.type = super.--(elems)

  override def + (elem: A): this.type = super.+(elem)
  override def + (elem1: A, elem2: A, elems: A*): this.type = super.+(elem1, elem2, elems: _*)
  override def ++ (elems: collection.Iterable[A]): this.type = super.++(elems)
  override def ++ (elems: Iterator[A]): this.type = super.++(elems)

  /** This method computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that the set to intersect with.
   */
  def intersect(that: Set[A]) { retain(that.contains) }

  /** Method <code>retain removes all elements from the set for
   *  which the predicate <code>p</code> yields the value <code>false</code>.
   */
  def retain(p: A => Boolean): Unit = foreach (elem => if (!p(elem)) -=(elem))

  /** Removes all elements from the set. After this operation is completed,
   *  the set will be empty.
   */
  def clear() { elements foreach -= }

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

  /** Return a read-only projection of this set !!! just us an (immutable) setProxy? */
  def readOnly : collection.Set[A] = new collection.Set[A] {
    def contains(item : A) = Set.this.contains(item)
    override def size = self.size
    override def +(elem: A) = self + elem
    override def -(elem: A) = self - elem
    override def elements = self.elements
    override def foreach(f: A => Unit) = self.foreach(f)
    override def newBuilder[B]: Builder[collection.Set, B] = self.newBuilder[B]
  }
}
