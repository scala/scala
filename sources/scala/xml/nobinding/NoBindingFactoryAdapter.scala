/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */
package scala.xml.nobinding;

import scala.collection.mutable.HashMap ;
import scala.collection.immutable.ListMap ;
import scala.xml.{Elem, Node,Text,FactoryAdapter,Utility} ;
import org.xml.sax.InputSource;

/** nobinding adaptor providing callbacks to parser to create elements.
*   implements hash-consing
*/
class NoBindingFactoryAdapter extends FactoryAdapter  {

  type Elem = scala.xml.Elem;

  // FactoryAdpater methods

  /** returns true. Every XML node may contain text that the application needs
  **/
  def nodeContainsText( label:java.lang.String ):boolean = true;

  /* default behaviour is to use hash-consing */
  val cache = new HashMap[int,Elem]();

  /** creates a node. never creates the same node twice, using hash-consing
  */
  def createNode( label: String, attrs: HashMap[String,String], children: List[Node] ):Elem = {

    val elHashCode = Utility.hashCode( label, attrs, children ) ;

    val attrList = attrs.toList;
    cache.get( elHashCode ).match{
      case Some(cachedElem) =>
        //System.err.println("[using cached elem +"+cachedElem.toXML+"!]"); //DEBUG
        cachedElem
      case None => val el = if( children.isEmpty ) {
       new Elem( label ) {
         private val hmap = attrs;
         override def attribute = attrs.elements.toSeq( attrs.size );
         override def hashCode() = elHashCode;
        };
      } else {
       new Elem( label, children:_* ) {
         private val hmap = attrs;
         override def attribute = attrs.elements.toSeq( attrs.size );
         override def hashCode() = elHashCode;
       };
      }
      cache.update( elHashCode, el );
      el
    }
  }

  /** creates a text node
  */
  def createText( text:String ) = Text( text );

  /** loads an XML document, returning a Symbol node.
  */
  override def loadXML( source:InputSource ):Elem =
    super.loadXML( source ).asInstanceOf[ Elem ]
}
