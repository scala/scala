/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml
package persistent

import scala.collection.mutable
import java.io.File

/** A persistent store with set semantics. This class allows to add and remove
 *  trees, but never contains two structurally equal trees.
 *
 *  @author Burak Emir
 */
class SetStorage(file: File) extends CachedFileStorage(file) {

  private val theSet = mutable.HashSet[Node]()

  // initialize

  {
    val it = super.initialNodes
    dirty = it.hasNext
    theSet ++= it
  }

  /* forwarding methods to hashset*/

  def += (e: Node): Unit = synchronized { this.dirty = true; theSet += e }

  def -= (e: Node): Unit = synchronized { this.dirty = true; theSet -= e }

  def nodes = synchronized { theSet.iterator }

}
