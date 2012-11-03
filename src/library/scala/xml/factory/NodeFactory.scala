/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml
package factory

import parsing.{ FactoryAdapter, NoBindingFactoryAdapter }
import java.io.{ InputStream, Reader, StringReader, File, FileDescriptor, FileInputStream }

trait NodeFactory[A <: Node] {
  val ignoreComments  = false
  val ignoreProcInstr = false

  /* default behaviour is to use hash-consing */
  val cache = new scala.collection.mutable.HashMap[Int, List[A]]

  protected def create(pre: String, name: String, attrs: MetaData, scope: NamespaceBinding, children:Seq[Node]): A

  protected def construct(hash: Int, old:List[A], pre: String, name: String, attrSeq:MetaData, scope: NamespaceBinding, children:Seq[Node]): A = {
    val el = create(pre, name, attrSeq, scope, children)
    cache.update(hash, el :: old)
    el
  }

  def eqElements(ch1: Seq[Node], ch2: Seq[Node]): Boolean =
    ch1.view.zipAll(ch2.view, null, null) forall { case (x,y) => x eq y }

  def nodeEquals(n: Node, pre: String, name: String, attrSeq:MetaData, scope: NamespaceBinding, children: Seq[Node]) =
    n.prefix == pre &&
    n.label == name &&
    n.attributes == attrSeq &&
    // scope?
    eqElements(n.child, children)

  def makeNode(pre: String, name: String, attrSeq: MetaData, scope: NamespaceBinding, children: Seq[Node]): A = {
    val hash = Utility.hashCode( pre, name, attrSeq.##, scope.##, children)
    def cons(old: List[A]) = construct(hash, old, pre, name, attrSeq, scope, children)

    (cache get hash) match {
      case Some(list) =>    // find structurally equal
        list.find(nodeEquals(_, pre, name, attrSeq, scope, children)) match {
          case Some(x)    => x
          case _          => cons(list)
        }
      case None           => cons(Nil)
    }
  }

  def makeText(s: String) = Text(s)
  def makeComment(s: String): Seq[Comment]  =
    if (ignoreComments) Nil else List(Comment(s))
  def makeProcInstr(t: String, s: String): Seq[ProcInstr] =
    if (ignoreProcInstr) Nil else List(ProcInstr(t, s))
}
