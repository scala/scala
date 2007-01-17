/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A concrete wrapper around a Java map. The identity of the wraper is
 *  fixed to the identity of the underlying map.
 *
 *  @author Sean McDirmid
 */
abstract class ConcreteMapWrapper[K,E] extends MapWrapper[K,E] {
  val underlying: java.util.Map;
  override def toString = underlying.toString;
  override def hashCode = underlying.hashCode;
  override def equals(that : Any) = that match {
    case that: ConcreteMapWrapper[_,_] => underlying == that.underlying;
    case _ => false;
  }
}
