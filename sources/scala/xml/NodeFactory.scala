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

  protected def construct(hash:Int, old:List[A], uname: UName, attrSeq:AttributeSeq, children:Seq[Node]): A = {
    val el = create(uname, attrSeq, children);
    cache.update( hash, el::old );
    el
  }

  /** faster equality, because */
  def eqElements(ch1:Seq[Node], ch2:Seq[Node]): Boolean = {
    (ch1.length == ch2.length) && {
      val it1 = ch1.elements;
      val it2 = ch2.elements;
      var res = true;
      while(res && it1.hasNext) {
        res = it1.next.eq(it2.next);
      }
      res
    }
  }

  def nodeEquals(n: Node, uname: UName, attrSeq:AttributeSeq, children:Seq[Node]) =
    (n.namespace == uname.uri)
    &&(n.label == uname.label)
    &&(n.attributes == attrSeq)
    &&(eqElements(n.child,children));

  def makeNode(uname: UName, attrSeq:AttributeSeq, children:Seq[Node]): A = {
    Console.println("wrong makeNode");
    val hash    = Utility.hashCode( uname, attrSeq.hashCode(), children ) ;
   cache.get( hash ) match {
      case Some(list) => // find structurally equal
        val it     = list.elements;
        val lookup = it.find { x => nodeEquals(x,uname,attrSeq,children) };
        lookup match {
          case Some(x) =>
            //Console.println("[cache hit !]"+x);
            x; // return cached elem
          case _       => construct(hash, list, uname, attrSeq, children);
        }
      case _          => construct(hash, Nil, uname, attrSeq, children)
    }
  }

  def makeText(s: String) =
    Text( s );

  def makeComment(s: String): Seq[Comment] =
    if(ignoreComments) Nil else List(Comment( s ));

  def makeProcInstr(t: String, s: String): Seq[ProcInstr] =
    if(ignoreProcInstr) Nil else List(ProcInstr(t, s));

  /*
  def makeCharData(s: String) =
    CharData( s );
  */

}
