/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */
package scala.xml ;

abstract class NodeFactory {

  val config:NodeFactoryConfig ;

  def makeNode( elemName:String, attribs:Map[String,String], chIter:Seq[Node] ):Node;

  def makeText( s:String )                 = Text( s );
  def makeComment( s:String ):Seq[Comment] =
    if(config.ignoreComments) Nil else List( Comment( s ) );
  def makeProcInstr( t:String, s:String )  = ProcInstr( t, s );
  def makeCharData( s:String )             = CharData( s );

}


case class NodeFactoryConfig(ignoreComments:boolean,namespace:Namespace ) ;
