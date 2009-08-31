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
import mutable.HashSet
import scala.io.File

/** A persistent store with set semantics. This class allows to add and remove
 *  trees, but never contains two structurally equal trees.
 *
 *  @author Burak Emir
 */
class SetStorage(file: File) extends CachedFileStorage(file)
{
  private var theSet: HashSet[Node] = HashSet() ++ initialNodes
  dirty = theSet.nonEmpty

  /* forwarding methods to hashset */

  def += (e: Node): Unit = synchronized { this.dirty = true; theSet += e }
  def -= (e: Node): Unit = synchronized { this.dirty = true; theSet -= e }
  def nodes = synchronized { theSet.iterator }
}
