/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A map that is backed by a Java linked hash map, which fixes iteration
 *  order in terms of insertion order.
 *
 *  @author Sean McDirmid
 */
class LinkedHashMap[K,E](override val underlying: java.util.LinkedHashMap) extends MapWrapper[K,E] {
  def this() = this(new java.util.LinkedHashMap);
}
