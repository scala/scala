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
    val cache = new HashMap[Element,Element];

    def createNode( label: String,
                    attrs: HashMap[String,String],
                    children: List[Node] ):Element = {

          val el = new Element( Symbol( label ), children ) {
            override val attribHashCode = attrs.toList.hashCode(); /*set eagerly*/
	    override def attributes = attrs;
          };

	  cache.get( el ).match{
	    case Some(cachedElem) =>
	      System.err.println("[using cached elem +"+cachedElem.toXML+" instead of "+el.toXML+"!]");
	      cachedElem
	    case None =>
	      cache.update( el, el );
	      el
	  }
    }

    def createText( text:String ) = Text( text );

    override def loadXML( url:URL ):Element = loadXML( url.getFile() );

    override def loadXML( filename:String ):Element =
      super.loadXML( filename ).asInstanceOf[ Element ]
  }
