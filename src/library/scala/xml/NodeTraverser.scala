/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

/** This class ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class NodeTraverser extends parsing.MarkupHandler {

  def traverse(n: Node): Unit = n match {
    case x:ProcInstr =>
      procInstr(0, x.target, x.text)
    case x:Comment =>
      comment(0, x.text)
    case x:Text =>
      text(0, x.data)
    case x:EntityRef =>
      entityRef(0, x.entityName)
    case _ =>
      elemStart(0, n.prefix, n.label, n.attributes, n.scope)
      for (m <- n.child) traverse(m)
      elem(0, n.prefix, n.label, n.attributes, n.scope, NodeSeq.fromSeq(n.child))
      elemEnd(0, n.prefix, n.label)
  }

}
