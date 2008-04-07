/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl

/** <p>
 *    A map that is backed by a Java weak hash map, whose keys are maintained
 *    as weak references.
 *  </p>
 *  <p>
 *    Because keys are weak references, the garbage collector can collect
 *    them if they are not referred to elsewhere.
 *  </p>
 *  <p>
 *    Useful for implementing caches.
 *  </p>
 *
 *  @author Sean McDirmid
 */
class WeakHashMap[K, E](override val underlying: java.util.WeakHashMap[K, E]) extends MapWrapper[K, E] {
  def this() = this(new java.util.WeakHashMap[K, E])
  override def clone: WeakHashMap[K, E] =
    throw new CloneNotSupportedException("The underlying map doesn't implement the Cloneable interface")
}
