/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** Describes wrappers around concrete underlying collections. The identity
 *  of the wrapper is related strictly to the Java collection being wrapped,
 *  which is structurally determined.
 *
 *  @author Sean McDirmid
 */
abstract class ConcreteWrapper[A] extends CollectionWrapper[A] {
  val underlying: java.util.Collection;
  override def elements : MutableIterator[A] = super.elements;
  override def toString = underlying.toString;
  override def hashCode = underlying.hashCode;
  override def equals(that : Any) = that match {
    case that: ConcreteWrapper[_] => underlying == that.underlying;
    case _ => false;
  }
}
