/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.factory;


trait NodeFactory[A <: Node] {

  val ignoreComments  = false;
  val ignoreProcInstr = false;

  /* default behaviour is to use hash-consing */
  val cache = new collection.mutable.HashMap[Int, List[A]]();

  protected def create(pre: String, name: String, attrs: MetaData, scope: NamespaceBinding, children:Seq[Node]): A;

  protected def construct(hash: Int, old:List[A], pre: String, name: String, attrSeq:MetaData, scope: NamespaceBinding, children:Seq[Node]): A = {
    val el = create(pre, name, attrSeq, scope, children);
    cache.update( hash, el::old );
    el
  }

  /** faster equality, because */
  def eqElements(ch1:Seq[Node], ch2:Seq[Node]): Boolean = {
    (ch1.length == ch2.length) && {
      val it1 = ch1.iterator;
      val it2 = ch2.iterator;
      var res = true;
      while(res && it1.hasNext) {
        res = it1.next.eq(it2.next);
      }
      res
    }
  }

  def nodeEquals(n: Node, pre: String, name: String, attrSeq:MetaData, scope: NamespaceBinding, children:Seq[Node]) = (
    (n.prefix == pre)
    &&(n.label == name)
    &&(n.attributes == attrSeq)
  // scope??
    &&(eqElements(n.child,children)));

  def makeNode(pre: String, name: String, attrSeq:MetaData, scpe: NamespaceBinding, children:Seq[Node]): A = {
    //Console.println("NodeFactory::makeNode("+pre+","+name+","+attrSeq+","+scpe+","+children+")");
    val hash    = Utility.hashCode( pre, name, attrSeq.hashCode(), scpe.hashCode(), children ) ;
    cache.get( hash ) match {
      case Some(list) => // find structurally equal
        val it     = list.iterator;
        val lookup = it.find { x => nodeEquals(x, pre, name, attrSeq, scpe, children) };
        lookup match {
          case Some(x) =>
            //Console.println("[cache hit !]"+x);
            x; // return cached elem
          case _       => construct(hash, list, pre, name, attrSeq, scpe, children);
        }
      case _          => construct(hash, Nil, pre, name, attrSeq, scpe, children)
    }
  }

  def makeText(s: String) =
    Text( s );

  def makeComment(s: String): Seq[Comment] =
    if(ignoreComments) Nil else List(Comment( s ));

  def makeProcInstr(t: String, s: String): Seq[ProcInstr] =
    if(ignoreProcInstr) Nil else List(ProcInstr(t, s));

}
