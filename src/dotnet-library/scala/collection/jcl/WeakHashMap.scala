/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

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
class WeakHashMap[K,E](override val underlying: java.util.WeakHashMap) extends MapWrapper[K,E] {
  def this() = this(new java.util.WeakHashMap);
}
