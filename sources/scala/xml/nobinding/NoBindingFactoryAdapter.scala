/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */
package scala.xml.nobinding;

import scala.collection.mutable;
import org.xml.sax.InputSource;

/** nobinding adaptor providing callbacks to parser to create elements.
*   implements hash-consing
*/
class NoBindingFactoryAdapter extends FactoryAdapter with NodeFactory[Elem] {


  // FactoryAdapter methods

  /** returns true. Every XML node may contain text that the application needs
  **/
  def nodeContainsText( label:java.lang.String ):boolean = true;


  // methods for NodeFactory[Elem]
  protected def create(pre: String, label: String, attrs: MetaData, scpe: NamespaceBinding, children:Seq[Node]): Elem = {
     Elem( pre, label, attrs, scpe, children:_* );
  }

  // methods for FactoryAdapter

  /** creates a node. never creates the same node twice, using hash-consing
  */
  def createNode(pre:String, label: String, attrs: MetaData, scpe: NamespaceBinding, children: List[Node] ): Elem = {
    //Console.println("NoBindingFactoryAdapter::createNode("+pre+","+label+","+attrs+","+scpe+","+children+")");
     Elem( pre, label, attrs, scpe, children:_* );

    //makeNode(pre, label, attrs, scope, children);
  }

  /** creates a text node
  */
  def createText( text:String ) =
    Text( text );

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
