/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package xml
package factory

import parsing.ValidatingMarkupHandler

/**
 *  @author Burak Emir
 */
abstract class Binder(val preserveWS: Boolean) extends ValidatingMarkupHandler {

  var result: NodeBuffer = new NodeBuffer()

  def reportSyntaxError(pos:Int, str:String) = {}

  final def procInstr(pos: Int, target: String, txt: String) =
    ProcInstr(target, txt)

  final def comment(pos: Int, txt: String) =
    Comment(txt)

  final def entityRef(pos: Int, n: String) =
    EntityRef(n)

  final def text(pos: Int, txt: String) =
    Text(txt)

  final def traverse(n:Node): Unit = n match {
    case x:ProcInstr =>
      result &+ procInstr(0, x.target, x.text)
    case x:Comment   =>
      result &+ comment(0, x.text)
    case x:Text      =>
      result &+ text(0, x.data)
    case x:EntityRef =>
      result &+ entityRef(0, x.entityName)
    case x:Elem =>
      elemStart(0, x.prefix, x.label, x.attributes, x.scope)
      val old = result
      result = new NodeBuffer()
      for (m <- x.child) traverse(m)
      result = old &+ elem(0, x.prefix, x.label, x.attributes, x.scope, x.minimizeEmpty, NodeSeq.fromSeq(result)).toList
      elemEnd(0, x.prefix, x.label)
  }

  final def validate(n: Node): Node = {
    this.rootLabel = n.label
    traverse(n)
    result(0)
  }
}
