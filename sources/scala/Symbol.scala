/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

import org.xml.sax.InputSource;

import scala.xml.{AttributedNode,Node, Text,Utility};
import scala.collection.immutable.Map;
import scala.collection.immutable.ListMap;


/** Instances of <code>Symbol</code> can be created easily with
 *  Scala's built-in quote mechanism. For instance, the Scala term
 *  <code>'mysym</code> will invoke the constructor of the
 *  <code>Symbol</code> class in the following way:
 *  <code>new Symbol("mysym", Nil)</code>. The Scala term <code>'mysym('foo,'bar)</code>
 *  will be treated as
 *  <code>new Symbol("mysym", List(new Symbol("foo",Nil), new Symbol("bar",Nil)</code>.
 *
 *  @author  Burak Emir, Martin Odersky
 *  @version 1.5, 08/12/2003
 */
case class Symbol(name: String, elems: Any*) extends AttributedNode {

    /** Returns the symbol name as a string.
     */
    def label: String = name;

    /** Returns the list of children of this symbol.
     */
    def children: Seq[Node] = List.fromIterator(elems.elements map ( x => x match {
        case n:Node => n;
        case _      => Text(x.toString());
    }));

    //private val attrMap: ListMap[String, String] = ListMap.Empty[String,String];

    /** Returns a map representing the attributes of this node.
     */
    def attributes: Map[String, String] = ListMap.Empty;

    /** Converts this symbol to a string
     */
    override def toString(): String = {
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

    /** returns a new symbol with updated attributes
     */
    final def %(attrs: List[Pair[String, String]]): Symbol = new Symbol( name, elems:_* ) {
      val themap = Symbol.this.attributes.incl( attrs );
      override def attributes = themap;
    }

    /** returns a new symbol with updated attribute
     */
    final def %(attr: Pair[String, String]): Symbol = new Symbol( name, elems:_* ) {
      val themap = Symbol.this.attributes.incl( attr );
      override def attributes = themap;
    }

    final def <=(value: String) = new Pair(name, value);

    final def saveTo(filename:String) = Symbol_IO.save(filename, this);
}

object Symbol_IO {
  // functions for generic symbol loading, saving

  /** loads symbol from a given file
   */
  def load( source:InputSource ):Symbol = scala.xml.nobinding.XML.load( source );

  /** saves symbol to filename (with encoding ISO-8859-1)
   */
  def save( filename:String, doc:Symbol ):Unit = scala.xml.nobinding.XML.save( filename, doc );

}
