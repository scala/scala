/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id $
\*                                                                      */

package scala.xml;

import scala.collection.Map ;
import scala.collection.mutable ;

abstract class NodeFactory[A <: Node] {

  val ignoreComments  = false;
  val ignoreProcInstr = false;

  /* default behaviour is to use hash-consing */
  val cache = new mutable.HashMap[int,List[A]]();



  protected def create(uname: UName, attrs: AttributeSeq, children:Seq[Node]): A;

  def makeNode(uname: UName, attrSeq:AttributeSeq, children:Seq[Node]): A = {
    val hash    = Utility.hashCode( uname, attrSeq.hashCode(), children ) ;

    def construct: A = {
        val el = create( uname, attrSeq, children );
        cache.update( hash, List(el));
        el
    }

    def nodeEquals(n: Node) =
      (n.namespace == uname.uri)
      &&(n.label == uname.label)
      &&(n.attributes == attrSeq)
      &&(n.child.sameElements(children));

    cache.get( hash ) match {
      case Some(list) => // find structurally equal
        val it     = list.elements;
        val lookup = it.find { x => nodeEquals(x) };
        lookup match {
          case Some(x) => x; // return cached elem
          case _       => construct;
        }
      case _          => construct
    }
  }

  def makeText(s: String) =
    Text( s );

  def makeComment(s: String): Seq[Comment] =
    if(ignoreComments) Nil else List(Comment( s ));

  def makeProcInstr(t: String, s: String): Seq[ProcInstr] =
    if(ignoreProcInstr) Nil else List(ProcInstr(t, s));

  def makeCharData(s: String) =
    CharData( s );


}
