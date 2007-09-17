package scala.xml.persistent

import scala.collection.mutable
import java.io.File
/** a persistent store with set semantics. This class allows to add and remove
  * trees, but never contains two structurally equal trees.
  */
class SetStorage(file: File) extends CachedFileStorage(file) {

  private var theSet: mutable.HashSet[Node] = new mutable.HashSet[Node]

  // initialize

  {
    val it = super.initialNodes
    dirty = it.hasNext
    for(val x <- it) {
      theSet += x;
    }
  }

  /* forwarding methods to hashset*/

  def += ( e:Node ): Unit = synchronized { this.dirty = true; theSet += e }

  def -= ( e:Node ): Unit = synchronized { this.dirty = true; theSet -= e }

  def nodes = synchronized { theSet.elements }

}
