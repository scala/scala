/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl

/** A map that is backed by a Java identity hash map, which compares keys
 *  by their reference-based identity as opposed to using equals and hashCode.
 *  An identity hash map will often perform better than traditional hash map
 *  because it can utilize linear probing.
 *
 *  @author Sean McDirmid
 */
class IdentityHashMap[K, E](override val underlying : java.util.IdentityHashMap[K, E]) extends MapWrapper[K, E] {
  def this() = this(new java.util.IdentityHashMap[K, E])
  override def clone: IdentityHashMap[K, E] =
    new IdentityHashMap[K, E](underlying.clone().asInstanceOf[java.util.IdentityHashMap[K, E]])
}
