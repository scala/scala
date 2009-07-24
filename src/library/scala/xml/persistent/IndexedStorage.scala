/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml
package persistent

import scala.collection.mutable
import java.io.File

/** indexed multiset of xml trees. The index may be an arbitrary totally
 * type, especially one can construct indices by selecting parts of
 * xml nodes.
 */
class IndexedStorage[A](file:  File, index: Index[A]) //@todo
extends CachedFileStorage(file) {

  private var theMap: mutable.Map[A,Node] = new mutable.HashMap[A,Node]()

  super.initialNodes.foreach { x:Node => this += x }

  this.dirty = false

  def += (e: Node): Unit = synchronized {
    log("added element at index '"+index(e)+"'")
    dirty = true
    theMap(index(e)) = e
  }

  def -= (e: Node): Unit = synchronized {
    log("removed element at index '"+index(e)+"'")
    dirty = true
    theMap -= index( e )
  }

  def nodes: Iterator[Node] = synchronized {
    theMap.valuesIterator
  }

  def lookup(n: A): Option[Node] = theMap.get(n)

}
