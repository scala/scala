/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

import scala.collection.mutable ;

abstract class NodeTraverser[A](handle: parsing.MarkupHandler[A]) {

  def traverse(n: Node): Iterable[A] = {
    val nb = new mutable.ArrayBuffer[A]();
    val it = n.child.elements;
    while(it.hasNext) {
      nb.appendAll(traverse(it.next));
    }
    handle.element(0, n.namespace, n.label, n.attributes.toMap, nb)
  }

}
