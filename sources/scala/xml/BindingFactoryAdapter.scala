/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */
package scala.xml ;

import scala.collection.Map ;
import scala.collection.mutable.HashMap ;

/** a FactoryAdapter that creates instances of classes binding XML types.
**   DTDs imported with the dtd2scala tool all use this class as interface
**   to the SAX XML parser, by giving concrete values for the factory maps f and g.
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
  def nodeContainsText( name:java.lang.String ):boolean = g( name );

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
        val c = f( elemName );
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
              val c = f( elemName );
              val el = c( children, attribs );
              cache.update( h, el );
              el
        }
      }
    } // createNode

   /** creates PCDATA element */
   def createText( text:String ):Text  = new Text( text );

} // BindingFactoryAdapter
