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

class NodeTraverser[A <: AnyRef](handle: parsing.MarkupHandler[A]) {

  def traverse(n: Node): Iterable[A] = n match {
    case Text(t)          => handle.text(0,t);
    case ProcInstr(ta,te) => handle.procInstr(0,ta,te);
    case Comment(t)       => handle.comment(0,t);
    case EntityRef(n)     => handle.entityRef(0,n);
    case _ =>
      val nb = new mutable.ArrayBuffer[A]();
      val it = n.child.elements;
      while(it.hasNext) {
        nb.appendAll(traverse(it.next));
      }
    handle.element(0, n.namespace, n.label, n.attributes.toMap, nb)
  }

}

class NodeSubstitution(handle: parsing.ConstructingHandler)
  extends NodeTraverser[Node](handle) with Function1[Node,Iterable[Node]] {
    def apply(n: Node) = traverse(n);
}
