package scala.xml ;

import scala.xml.javaAdapter.Map ;
import scala.xml.javaAdapter.HashMap ;

/** a Scala specific dtd2scala.FactoryAdapter, which plays the SAX content handler for the SAX parser
 *  It implements the three callback methods elementContainsText, createElement and createPCDATA.
 *  DTDs imported with the dtd2scala tool all use this class as interface to the SAX XML parser, by
 *  giving concrete values for the factory maps f and g.
 */

abstract class ScalaFactoryAdapter
    extends dtd2scala.FactoryAdapter()  {

    def iterToList[ a ]( iter:java.util.Iterator ):List[a] =
        if( !iter.hasNext() )
            Nil
        else
            (iter.next() as a )::iterToList( iter ) ;


    def mapToMap[a,b]( map:java.util.Map ):Map[a,b] = {

         val keys:java.util.Iterator = map.keySet().iterator();
         val res = new HashMap[a,b] ;

         def iterToMap:Unit =
         if( keys.hasNext() ) {
              val key   = keys.next();
              val value = map.get( key ) as b ;
              res.put( key as a , value as b );
              iterToMap
         } else
              () ;

        iterToMap;
        res
    }

    /** a mapping from a element name (string) to an element constructor (constr:Seq[Element] => Element)
     */
    val f: Map[ String, Seq[Element] => Element ];

    /** a mapping from an element name (string) to a truth value indicating whether text (PCDATA) may appear as
      */

    val g: Map[ String, boolean ] ;

    val compress: boolean ;

    def   elementContainsText( name:java.lang.String ):boolean =
         g.get( name ) ;

    // if compress is set, used for hash-consing
    val cache = new HashMap();

    def   createElement(elemName:String,
                        attribs:java.util.Map,
                        children:java.util.Iterator ):scala.Object = {
	  val _children = iterToList[Element]( children ); // 2do:optimize
	  if( !compress ) {
            val c = f.get( elemName ); // get constructor
            val el = c( _children );
            el.setAttribs( mapToMap[String,String]( attribs ) );
	    el
	  } else { // do hash-consing

	    val h = Element.hashValue( elemName, attribs, _children );
	    val el_cache = cache.get( h as scala.All ) as scala.Object;
	    if ( el_cache != null ) {  // return cached elem
	      el_cache
	    } else {
	      val c = f.get( elemName ); // get constructor
              val el = c( _children );
              el.setAttribs( mapToMap[String,String]( attribs ) );
	      cache.put( h as scala.All, el as scala.All );
	      el
	    }
	  }
    }

    def createPCDATA( text:String ):scala.Object  = {
          new PCDATA( text );
    };

} // ScalaFactoryAdapter
