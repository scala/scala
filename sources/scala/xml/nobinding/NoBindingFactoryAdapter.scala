/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */
package scala.xml.nobinding;

import scala.collection.{ immutable, mutable };
//import scala.xml.{ Elem, Node, Text, FactoryAdapter, Utility };
import org.xml.sax.InputSource;

/** nobinding adaptor providing callbacks to parser to create elements.
*   implements hash-consing
*/
class NoBindingFactoryAdapter extends FactoryAdapter with NodeFactory[Elem] {

  type Elem = scala.xml.Elem;

  // FactoryAdapter methods

  /** returns true. Every XML node may contain text that the application needs
  **/
  def nodeContainsText( label:java.lang.String ):boolean = true;


  // methods for NodeFactory[Elem]
  protected def create(uname: UName, attrs: AttributeSeq, children:Seq[Node]): Elem = {
     Elem( uname.uri, uname.label, attrs, children:_* );
  }

  // methods for FactoryAdapter

  /** creates a node. never creates the same node twice, using hash-consing
  */
  def createNode(uri:String, label: String, attrs: mutable.HashMap[Pair[String,String],String], children: List[Node] ): Elem = {

    // this is a dirty hack to quickly add xmlns.
    // does SAX handle prefixes, xmlns stuff ?
    val defNS = getDefaultNamespace( attrs.keys );
    var uri$:String = if(( defNS.length() > 0 )&&( uri.length == 0 )) {
      defNS
    } else {
      uri.intern();
    }
    val attrSeq = AttributeSeq.fromMap( uri$, attrs );
    val elHashCode =
      Utility.hashCode( uri$, label, attrSeq.hashCode(), children ) ;
    makeNode(UName(uri$,label),attrSeq, children);
  }

  /** creates a text node
  */
  def createText( text:String ) = Text( text );

  /** loads an XML document, returning a Symbol node.
  */
  override def loadXML( source:InputSource ):Elem =
    super.loadXML( source ).asInstanceOf[ Elem ];

  def getDefaultNamespace(it:Iterator[Pair[String,String]]):String = {
    while( it.hasNext ) {
      val key = it.next;
      if( key._1 == "xmlns")
        return key._2
    }
    return "";
  }
                        }
