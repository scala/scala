/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

import scala.xml.{AttributedNode,Node, Text,Utility};
import scala.collection.Map ;
import scala.collection.immutable.ListMap ;

/** Instances of <code>Symbol</code> can be created easily with
 *  Scala's built-in quote mechanism. For instance, the Scala term
 *  <code>'mysym</code> will invoke the constructor of the
 *  <code>Symbol</code> class in the following way:
 *  <code>new Symbol("mysym", Nil)</code>. The Scala term <code>'mysym('foo,'bar)</code>
  *  will be treated as <code>new Symbol("mysym", List(new Symbol("foo",Nil), new Symbol("bar",Nil)</code>
 *
 *  @author  Burak Emir, Martin Odersky
 *  @version 1.5, 2003-12-08
 */
case class Symbol(name: String, elems: Any*) extends AttributedNode {

  // Node methods
  def label = name;
  def children:Seq[Node] = List.fromIterator( elems.elements map ( x => x match {
    case n:Node => n;
    case _      => Text( x.toString() );
  }));

  var attrMap:ListMap[String,String] = ListMap.Empty[String,String];

  // AttributedNode methods, duplicated because of mixin misery
  override def attributes : Map[String,String] = attrMap;

  /** converts this symbol to a string */
  override def toString():String = {
    val s = new StringBuffer("'" + name) ;
    val it = elems.elements;
    if( it.hasNext ) {
      s.append("(");
      val e1 = it.next.toString();
      s.append( e1 );
      for( val e <- it ) {
       s.append(','); s.append( e.toString() );
      }
      s.append(")");
    }
    s.toString();
  }

  /** this method <b>destructively</b> updates attributes of this symbol.
  */
  final def % (attrs:List[AttrDef]):Symbol = {
    attrMap = ListMap.Empty[String,String];
    for( val a <- attrs.elements ) {
      attrMap = attrMap.update( a.key, a.value );
    }
    this
  }

  final def <=( value:String ) = new AttrDef( name, value );

  case class AttrDef( key:String, value:String );

  final def saveTo( filename:String ) = Symbol_IO.save( filename, this );

}

object Symbol_IO {
  // functions for generic symbol loading, saving

  /** loads symbol from a given file*/
  def load( filename:String ):Symbol = scala.xml.nobinding.XML.load( filename );

  /** saves symbol to filename (with encoding ISO-8859-1) */
  def save( filename:String, doc:Symbol ):Unit = scala.xml.nobinding.XML.save( filename, doc );

}
