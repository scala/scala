package scala.xml ;

import scala.collection.Map ;
import scala.collection.mutable.HashMap ;
//import scala.xml.javaAdapter.Map ;
//import scala.xml.javaAdapter.HashMap ;

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

    def elementContainsText( name:java.lang.String ):boolean =
         g.get( name ).match { case Some(x) => x };

    // if compress is set, used for hash-consing
    val cache = new HashMap[int,Element];
    //var cacheCount = 0;
   /** creates an element. uses hash-consing if compress == true
    */
   def   createElement(elemName:String,
                       attribs:HashMap[String,String],
                       children:List[Element] ):Element = {
      // 2do:optimize
      if( !compress ) {
        // get constructor
        val c = f.get( elemName ).match{ case Some(x) => x };
        val el = c( children );
        el.setAttribs( attribs );
        el
      } else { // do hash-consing

	val h = Element.hashValue( elemName, attribs, children );
        cache.get( h ).match {

            case Some(cachedElem) =>
	      //cacheCount = cacheCount + 1;
	      //System.err.println("[ScalaFactoryAdapter: cache hit "+cacheCount+"]");
	      cachedElem

            case None =>
              // get constructor
              val c = f.get( elemName ).match{ case Some(x) => x };
              val el = c( children );
              el.setAttribs( attribs );
              cache.update( h, el );
              el
        }
      }
    } // createElement

   /** creates PCDATA element */
   def createPCDATA( text:String ):PCDATA  = new PCDATA( text );

} // ScalaFactoryAdapter
