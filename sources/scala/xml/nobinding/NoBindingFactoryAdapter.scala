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
import scala.xml.{ Elem, Node, Text, FactoryAdapter, Utility };
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
  val cache = new mutable.HashMap[int,Elem]();

  /** creates a node. never creates the same node twice, using hash-consing
  */
  def createNode(uri:String,
                 label: String,
                 attrs: mutable.HashMap[String,String],
                 children: List[Node] ):Elem = {

    val uri$ = uri.intern();
    val elHashCode = Utility.hashCode( uri$, label, attrs, children ) ;

    val attrMap = immutable.TreeMap.Empty[String,String] incl attrs;
    cache.get( elHashCode ).match{
      case Some(cachedElem) =>
        //System.err.println("[using cached elem +"+cachedElem.toXML+"!]"); //DEBUG
        cachedElem
      case None =>
      val el = Elem( uri$, label, attrMap, children:_* );
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
