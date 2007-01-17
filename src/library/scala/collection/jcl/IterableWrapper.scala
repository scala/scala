/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A wrapper around a Java collection that only supports remove mutations.
 *
 *  @author Sean McDirmid
 */
trait IterableWrapper[A] extends MutableIterable[A] {
  protected def underlying: java.util.Collection;
  override def remove(a: A) = underlying.remove(a);
  override def removeAll(that: Iterable[A]) = that match {
    case that: IterableWrapper[_] => underlying.removeAll(that.underlying);
    case _ => super.removeAll(that);
  }
  override def retainAll(that : Iterable[A]) = that match {
    case that : IterableWrapper[_] => underlying.retainAll(that.underlying);
    case _ => super.retainAll(that);
  }
  override def size = underlying.size;
  override def isEmpty = underlying.isEmpty;
  override def clear = underlying.clear;
  override def elements : MutableIterator[A] = new IteratorWrapper(underlying.iterator);
  class IteratorWrapper(underlying : java.util.Iterator) extends MutableIterator[A] {
    // val underlying = IterableWrapper.this.underlying.iterator;
    def hasNext = underlying.hasNext;
    def next = underlying.next.asInstanceOf[A];
    def remove = underlying.remove;
  }

}
