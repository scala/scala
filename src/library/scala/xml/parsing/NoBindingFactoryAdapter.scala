/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.parsing;


import scala.xml.factory.NodeFactory;
import org.xml.sax.InputSource;

/** nobinding adaptor providing callbacks to parser to create elements.
*   implements hash-consing
*/
class NoBindingFactoryAdapter extends FactoryAdapter with NodeFactory[Elem] {

  // -- FactoryAdapter methods

  /** returns true. Every XML node may contain text that the application needs
  **/
  def nodeContainsText( label:java.lang.String ): Boolean = true;


  // methods for NodeFactory[Elem]

  /** constructs an instance of scala.xml.Elem */
  protected def create(pre: String, label: String, attrs: MetaData, scpe: NamespaceBinding, children:Seq[Node]): Elem = {
     Elem( pre, label, attrs, scpe, children:_* );
  }

  // -- methods for FactoryAdapter

  /** creates a node. never creates the same node twice, using hash-consing
  */
  def createNode(pre:String, label: String, attrs: MetaData, scpe: NamespaceBinding, children: List[Node] ): Elem = {
    //Console.println("NoBindingFactoryAdapter::createNode("+pre+","+label+","+attrs+","+scpe+","+children+")");
    Elem( pre, label, attrs, scpe, children:_* );
    //makeNode(pre, label, attrs, scpe, children);
  }

  /** creates a text node
  */
  def createText( text:String ) =
    Text( text );

  /** loads an XML document, returning a Symbol node.
  */
  override def loadXML( source:InputSource ):Elem =
    super.loadXML( source ).asInstanceOf[ Elem ];

}
