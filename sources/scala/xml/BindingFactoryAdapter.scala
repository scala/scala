package scala.xml ;

import scala.collection.Map ;
import scala.collection.mutable.HashMap ;
//import scala.xml.javaAdapter.Map ;
//import scala.xml.javaAdapter.HashMap ;

/** a Scala specific dtd2scala.FactoryAdapter, which plays the SAX content
*   handler for the SAX parser. It implements the three callback methods
*   elementContainsText, createNode and createPCDATA. DTDs imported with
*   the dtd2scala tool all use this class as interface to the SAX XML parser,
*   by giving concrete values for the factory maps f and g.
*/

abstract class BindingFactoryAdapter extends FactoryAdapter() {

  /** mapping from element names to an element constructor
    *  (constr:Seq[Node],HashMap[String,String] => Node)
    */
  val f: Map[ String, (Seq[Node],HashMap[String,String]) => Node ];

  /** mapping from element names to a truth value indicating
  * whether the corresponding element may have text children
  */
  val g: Map[ String, boolean ] ;

  /** if true, create only elements not created before */
  val compress: boolean ;

  /** looks up whether an element may have text children */
  def nodeContainsText( name:java.lang.String ):boolean =
    g.get( name ).match { case Some(x) => x };

  // if compress is set, used for hash-consing
  val cache = new HashMap[int,Node];
  //var cacheCount = 0;

  /** creates an element. see also compress */
  def   createNode(elemName:String,
                   attribs:HashMap[String,String],
                   children:List[Node] ):Node = {
      // 2do:optimize
      if( !compress ) {
        // get constructor
        val c = f.get( elemName ).match{ case Some(x) => x };
        c( children, attribs );
      } else { // do hash-consing

        val ahc = attribs.toList.hashCode();
	val h = Utility.hashCode( elemName, ahc, children );
        cache.get( h ).match {

            case Some(cachedElem) =>
	      //cacheCount = cacheCount + 1;
	      //System.err.println("[ScalaFactoryAdapter: cache hit "+cacheCount+"]");
	      cachedElem

            case None =>
              // get constructor
              val c = f.get( elemName ).match{ case Some(x) => x };
              val el = c( children, attribs );
              cache.update( h, el );
              el
        }
      }
    } // createNode

   /** creates PCDATA element */
   def createText( text:String ):Text  = new Text( text );

} // BindingFactoryAdapter
