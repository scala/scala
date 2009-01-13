/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** Used to wrap Java sets.
 *
 *  @author Sean McDirmid
 */
trait SetWrapper[A] extends Set[A] with CollectionWrapper[A] {
  def underlying: java.util.Set[A];
  override def isEmpty = super[CollectionWrapper].isEmpty;
  override def clear() = super[CollectionWrapper].clear;
  override def size = underlying.size;
}
