/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** Used to wrap Java collections in Scala.
 *
 *  @author Sean McDirmid
 */
trait CollectionWrapper[A] extends Collection[A] with IterableWrapper[A] {
  /** Override to specify the collection being accessed through this wrapper.
   ** Collection operations are then routed through the wrapped Java collection.
   **/
  def underlying : java.util.Collection[A];
  override def has(a : A) = underlying.contains(a);
  override def elements : MutableIterator[A] = super.elements;
  override def size = underlying.size;

  override def hasAll(that : Iterable[A]) = that match {
  case that : CollectionWrapper[_] =>
    val u = underlying;
    u.containsAll(that.underlying);
  case _ => super.hasAll(that);
  }
  override def add(a : A) = underlying.add(a);
  override def addAll(that : Iterable[A]) = that match {
  case that : CollectionWrapper[_] => underlying.addAll(that.underlying);
  case _ => super.addAll(that);
  }
  override def toString = underlying.toString;
  override def hashCode = underlying.hashCode;
  override def equals(that : Any) = that match {
    case that: CollectionWrapper[_] => underlying == that.underlying;
    case _ => super.equals(that);
  }
}
