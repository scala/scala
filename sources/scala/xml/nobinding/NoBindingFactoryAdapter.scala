package scala.xml.nobinding;

import scala.collection.mutable.HashMap ;
import scala.xml.{Node,Text,FactoryAdapter,Utility} ;
import org.xml.sax.InputSource;

/** nobinding adaptor providing callbacks to parser to create elements.
*   implements hash-consing
*/
class NoBindingFactoryAdapter extends FactoryAdapter  {

  def nodeContainsText( label:java.lang.String ):boolean = true;

  /* default behaviour is hash-consing */
  val cache = new HashMap[int,Symbol]();

  def createNode( label: String, attrs: HashMap[String,String], children: List[Node] ):Symbol = {

    val elHashCode = Utility.hashCode( label, attrs, children ) ;

    cache.get( elHashCode ).match{
      case Some(cachedElem) =>
        //System.err.println("[using cached elem +"+cachedElem.toXML+"!]");
      cachedElem
      case None => val el = if( children.isEmpty ) {
       new Symbol( label ) {
          override def attributes = attrs;
          override def hashCode() = Utility.hashCode( label, attrs.toList.hashCode(), children );
        };
      } else {
       new Symbol( label, children:_* ) {
          override def attributes = attrs;
          override def hashCode() = Utility.hashCode( label, attrs.toList.hashCode(), children );
        };
      }
      cache.update( elHashCode, el );
      el
    }
  }

  def createText( text:String ) = Text( text );

  override def loadXML( source:InputSource ):Symbol =
    super.loadXML( source ).asInstanceOf[ Symbol ]
}
