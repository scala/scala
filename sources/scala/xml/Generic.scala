package scala.xml;

import scala.collection.Map ;
import scala.collection.mutable.HashMap ;

/** Generic.load( <fileName> ) will load the xml document from file and
 *  create a tree with scala.Labelled, PCDATA and scala.Symbol objects.
 *  Text can appear within PCDATA at the leaves.
 */

object Generic {

          // utility functions

  /** TEMPORARY converting Java iterators to scala List

    def iterToList[ a ]( iter:java.util.Iterator ):List[a] =
        if( !iter.hasNext() )
            Nil
        else
            (iter.next().asInstanceOf[ a ])::iterToList( iter ) ;

  */

  /** TEMPORARY converting Java maps to javaAdapter maps

    def mapToMap[a,b]( map:java.util.Map ):Map[a,b] = {

         val keys:java.util.Iterator = map.keySet().iterator();
         val res = new HashMap[a,b] ;

         def iterToMap:Unit =
         if( keys.hasNext() ) {
              val key   = keys.next();
              val value = map.get( key ).asInstanceOf[ b ];
              res.put( key.asInstanceOf[ a ], value.asInstanceOf[ b ]);
              iterToMap
         } else
              () ;

        iterToMap;
        res
    }
  */

  /** turns a Map that contains attributes into XML like att1="val1" att2="val2"
  */

    def toXML( attrib:Map[ String, String ] ):String = {
	def iterate( keys:Iterator[String] ) =
	    if( keys.hasNext ) {
                        val key = keys.next;
			" " + key + "=\"" + attrib.get( key ).match{ case Some(x) => x } + "\"";
            } else {
			""
	    }

	if( attrib != null ) iterate( attrib.keys.elements ) else "";
    }

  //  attributes

  /** this trait is mixed in with Labelled in order to provide attributes
   */

  trait Attribbed {

    // only CDATA / String attributes for now
    def attribs : Map[String,String] ;

  }
  // functions for generic xml loading, saving

  /** will load the given file and return a generic XML representation
  */

  def load( filename:String ):Labelled = {
    val b = new GenericFactoryAdapter().loadXML( filename );
    b.asInstanceOf[Labelled]
  };

  /** will save a generic XML representation doc to filename. Uses NIO classes of JDK 1.4 for character conversion,
  *   encoding is fixed to ISO-8859-1 for now.
  */

  def save( filename:String, doc:Any ):Unit = {
    import java.io.{FileOutputStream,Writer};
    import java.nio.channels.{Channels,FileChannel};
    def toXMLList( xs: List[Any], fc:Writer ):Unit = xs match {
        case _::ys =>
                toXML( xs.head, fc );
                toXMLList( ys, fc );
        case _ => ()
    }
    def toXML( doc: Any, fc:Writer ):Unit = doc match {
        case PCDATA( s ) =>
                fc.write( (doc.asInstanceOf[ PCDATA ]).toXML );
        case Labelled( Symbol( tag ), xs ) =>
                fc.write( "<" );
                fc.write( tag );
                fc.write( Generic.toXML(( doc.asInstanceOf[ Attribbed ])
					.attribs ));
                fc.write( ">" );
                toXMLList( xs, fc );
                fc.write( "</" );
                fc.write( tag );
                fc.write( ">" );

    }
    val fos = new FileOutputStream( filename );
    val w = Channels.newWriter( fos.getChannel(), "ISO-8859-1" );
    toXML( doc, w );
    w.close();
    fos.close();
  }

  /** this class contains methods called back by the parser to create elements.
  *   It implements hash-consing, i.e. identical elemens (same tag, same attributes, same children)
  *   are only constructed once !
  */

  class GenericFactoryAdapter extends FactoryAdapter()  {

    def   elementContainsText( name:java.lang.String ):boolean = true;

    // default behaviour is hash-consing
    val cache = new HashMap[Element,Element];

    def   createElement( elemName: String,
                         attrs: HashMap[String,String],
                         children: List[Element] ):Element = {

          val el = new Labelled( Symbol( elemName ), children )
                with Attribbed with Element {
		     def getName = elemName;
		     def getChildren = children;
                     def attribs = attrs ; // ?! not needed anymore
		     def getAttribs = attrs;
		     def setAttribs( m:Map[ String, String ] ) = {
		       /* FIXME throw error */ };
                };

	  cache.get( el ).match{
	    case Some(cachedElem) =>
	      System.err.println("[using cached elem!]");
	      cachedElem
	    case None =>
	      cache.update( el, el );
	      el
	  }
    }; // createElement

    def createPCDATA( text:String ):PCDATA  = {
          new PCDATA( text );
    };

  } // GenericFactoryAdapter

} //Generic
