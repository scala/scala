package scala.xml.nobinding;

import java.net.URL;
import scala.collection.mutable.HashMap ;
import scala.xml.{Node,Text,FactoryAdapter} ;

/** nobinding adaptor providing callbacks to parser to create elements.
*   implements hash-consing
*/
class NoBindingFactoryAdapter extends FactoryAdapter  {

  def nodeContainsText( label:java.lang.String ):boolean = true;

  /* default behaviour is hash-consing */
  val cache = new HashMap[int,Element]();

  def createNode( label: String, attrs: HashMap[String,String], children: List[Node] ):Element = {

    val elHashCode = Utility.hashCode( label, attrs, children ) ;

    cache.get( elHashCode ).match{
      case Some(cachedElem) =>
        //System.err.println("[using cached elem +"+cachedElem.toXML+"!]");
      cachedElem
      case None =>
        val el = new Element( label, children ) {
          override def attributes = attrs;
        };
      cache.update( elHashCode, el );
      el
    }
  }

  def createText( text:String ) = Text( text );

  override def loadXML( url:URL ):Element = loadXML( url.getFile() );

  override def loadXML( filename:String ):Element =
    super.loadXML( filename ).asInstanceOf[ Element ]
}
