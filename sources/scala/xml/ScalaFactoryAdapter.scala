package scala.xml ;

import scala.xml.javaAdapter.Map ;
import scala.xml.javaAdapter.HashMap ;

/** a Scala specific dtd2scala.FactoryAdapter, which plays the SAX content
*   handler for the SAX parser. It implements the three callback methods
*   elementContainsText, createElement and createPCDATA. DTDs imported with
*   the dtd2scala tool all use this class as interface to the SAX XML parser,
*   by giving concrete values for the factory maps f and g.
*/

abstract class ScalaFactoryAdapter
    extends FactoryAdapter() {

    /** subclasses need to provide a mapping from a element name (string) to an element constructor
      *  (constr:Seq[Element] => Element)
      */
    val f: Map[ String, Seq[Element] => Element ];

    /** subclasses need to provide a mapping from an element name (string) to a truth value indicating
      * whether text (PCDATA) may appear as
      */

    val g: Map[ String, boolean ] ;

      /** subclasses need to tell whether they want to use hash-consing, which creates only non-identical elements
      */
      val compress: boolean ;

    /** looks up in g whether an element may contain text (PCDATA)
      */

    def   elementContainsText( name:java.lang.String ):boolean =
         g.get( name ) ;

    // if compress is set, used for hash-consing
    val cache = new HashMap();

      /** creates an element. uses hash-consing if compress == true
      */
      def   createElement(elemName:String,
			  attribs:java.util.Map,
			  children:java.util.Iterator ):scala.Object = {
			    val _children = Generic.iterToList[Element]( children ); // 2do:optimize
			    if( !compress ) {
			      val c = f.get( elemName ); // get constructor
			      val el = c( _children );
			      el.setAttribs( Generic.mapToMap[String,String]( attribs ) );
			      el
			    } else { // do hash-consing

			      val h = Element.hashValue( elemName, attribs, _children );
			      val el_cache = cache.get( h.asInstanceOf[scala.All] ).asInstanceOf[scala.Object];
			      if ( el_cache != null ) {  // return cached elem
				el_cache
			      } else {
				val c = f.get( elemName ); // get constructor
				val el = c( _children );
				el.setAttribs( Generic.mapToMap[String,String]( attribs ) );
				cache.put( h.asInstanceOf[scala.All], el.asInstanceOf[scala.All] );
				el
			      }
			    }
			  }

      /** creates PCDATA element */
      def createPCDATA( text:String ):scala.Object  = {
        new PCDATA( text );
      };

} // ScalaFactoryAdapter
