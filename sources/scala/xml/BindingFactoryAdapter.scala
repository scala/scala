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

abstract class BindingFactoryAdapter extends FactoryAdapter() with NodeFactory[Node] {

  val namespace:String;
  var errors = 0;

  /** mapping from element names to an element constructor
    *  (constr:Seq[Node],HashMap[String,String] => Node)
    */
  val f: Map[ String, (AttributeSeq,Seq[Node]) => Node ];

  /** mapping from element names to a truth value indicating
  * whether the corresponding element may have text children
  */
  val g: Map[ String, boolean ] ;

  /** if true, create only elements not created before */
  val compress: boolean ;

  /** looks up whether an element may have text children */
  def nodeContainsText( name:java.lang.String ):boolean = {
    g.get( name ) match {
      case Some(x) => x;
      case _       =>
        errors = errors + 1;
        java.lang.System.err.println(
          "[unrecognized element \""+name+"\"]"
        );true
    }
  }

  // if compress is set, used for hash-consing
  //val cache = new HashMap[int,Node];
  //var cacheCount = 0;


  def getConstructor(elemName: String) =
    f.get( elemName ) match {
      case Some(d) => d
      case _       => {
        throw new IllegalArgumentException("unrecognized:"+elemName);
      }
    }

  protected def create(uname: UName, attrs: AttributeSeq, children:Seq[Node]): Node = {
    if( this.namespace == uname.uri ) {
      val c = getConstructor( uname.label );
      c( attrs, children );
    } else {
      Elem( uname.uri, uname.label, attrs, children:_* );
    }
  }

  /** creates an element. see also compress */
  def   createNode(uri:String,
                   elemName:String,
                   attribs:HashMap[Pair[String,String],String],
                   children:List[Node] ):Node = {
      val uri$ = uri.intern();
      val attribs1 = AttributeSeq.fromMap(attribs);

      // 2do:optimize
      if( !compress ) {
        // get constructor
        val c = getConstructor(elemName);
        c( attribs1, children );
      } else { // do hash-consing

        val ahc = attribs.toList.hashCode();
        makeNode(UName(uri$, elemName), attribs1, children);
        /*
	val h = Utility.hashCode( uri$, elemName, ahc, children );
        cache.get( h ).match {

            case Some(cachedElem) =>
	      //cacheCount = cacheCount + 1;
	      //System.err.println("[ScalaFactoryAdapter: cache hit "+cacheCount+"]");
	      cachedElem

            case None =>
              // get constructor
              val c = getConstructor( elemName );
              val el = c( attribs1, children );
              cache.update( h, el );
              el
        }
        */
      }
    } // createNode

   /** creates PCDATA element */
   def createText( text:String ):Text  = new Text( text );

} // BindingFactoryAdapter
