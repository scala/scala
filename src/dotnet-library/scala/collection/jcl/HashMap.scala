/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A map that is backed by a Java hash map.
 *
 *  @author Sean McDirmid
 */
class HashMap[K,E](override val underlying: java.util.HashMap) extends MapWrapper[K,E] {
  def this() = this(new java.util.HashMap);
}
