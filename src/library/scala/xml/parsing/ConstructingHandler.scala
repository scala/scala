/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml
package parsing

/** Implementation of MarkupHandler that constructs nodes.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class ConstructingHandler extends MarkupHandler
{
  val preserveWS: Boolean

  def elem(pos: Int, pre: String, label: String, attrs: MetaData,
           pscope: NamespaceBinding, empty: Boolean, nodes: NodeSeq): NodeSeq =
    Elem(pre, label, attrs, pscope, empty, nodes:_*)

  def procInstr(pos: Int, target: String, txt: String) =
    ProcInstr(target, txt)

  def comment(pos: Int, txt: String)  = Comment(txt)
  def entityRef(pos: Int, n: String)  = EntityRef(n)
  def text(pos: Int, txt: String)     = Text(txt)
}
