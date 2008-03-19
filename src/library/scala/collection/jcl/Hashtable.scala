/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl

/** A hash set that is backed by a Java hash table.
 *
 *  @author Sean McDirmid
 */
class Hashtable[K,E](override val underlying: java.util.Hashtable[K,E]) extends MapWrapper[K,E] {
  def this() = this(new java.util.Hashtable[K,E])

  override def clone() : Hashtable[K,E] =
    new Hashtable[K,E](underlying.clone().asInstanceOf[java.util.Hashtable[K,E]])
}
