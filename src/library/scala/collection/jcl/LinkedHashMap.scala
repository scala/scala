/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl

/** A map that is backed by a Java linked hash map, which fixes iteration
 *  order in terms of insertion order.
 *
 *  @author Sean McDirmid
 */
class LinkedHashMap[K, E](override val underlying: java.util.LinkedHashMap[K, E]) extends MapWrapper[K, E] {
  def this() = this(new java.util.LinkedHashMap[K, E])
  override def clone: LinkedHashMap[K, E] =
    new LinkedHashMap[K, E](underlying.clone().asInstanceOf[java.util.LinkedHashMap[K, E]])
}
