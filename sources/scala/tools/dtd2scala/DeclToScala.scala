import scalac.util.Name ;
import scala.tools.scalac.ast.parser.Tokens ; /* for keywords */

package scala.tools.dtd2scala {

import java.io.PrintWriter ;
import org.xml.sax.InputSource ;

import scala.collection.Map ;
import scala.collection.mutable.HashMap ;

import scala.xml._ ;
import scala.xml.nobinding.XML ;

/** transforms a set of DTD declaraion to a scala source file.
 *  2do: parameterize with destination package.
 */
class DeclToScala(fOut:PrintWriter,
		  moduleName:String,
		  elemMap:Map[ String, ElemDecl ] ) {

  abstract class objectTemplate {
    val objectName : String = "myXML"; /* DEFAULT MODULE NAME */
    val package_ : String = "";
    val compress : boolean = true;
    final val tmplFile = "scala/tools/dtd2scala/template/ObjectTemplate.scala.xml";
    final val tmpl:Node = XML.load( new InputSource(ClassLoader.getSystemResourceAsStream(tmplFile)) );

    val lookup : HashMap[String,String] = new HashMap[String,String]();
    var curAttribs: Map[String,AttrDecl] = null ;  /* of current elem */

    def write:Unit = {
      def writeNode( x:Node ):Unit = {
        //Console.println("visiting "+x);
        x match {
          case Text(text) =>
            fOut.print( text );
          case n:Elem =>
            n.label match {
              case "template" => {
                lookup.update("objectName", objectName);
                lookup.update("compressDefault", compress.toString());
                n.children.elements.foreach { n => writeNode(n) }
              }
              case "elementBinding" => {
                for( val decl <- elemMap.values.elements ) {
                  lookup += "elementName" -> decl.name;
                  lookup += "elementContainsText" -> decl.containsText.toString();
                  lookup += "elementContentModel" -> decl.contentModel;
                  curAttribs = decl.attribs;
                  n.children.elements.foreach{ n => writeNode( n ) }
                }
                curAttribs = null;
                lookup -= "elementName";
                lookup -= "elementContainsText";
              }
              /*
              case "attributeAssign" => {
                for( val aDecl <- curAttribs.keys.elements ) {
                  lookup += "attributeName" -> aDecl;
                  n.children.elements.foreach{ n => writeNode( n ) }
                }
                lookup -= "attributeName";
              }
              */
              case "attributeBinding" => {
                for( val aDecl <- curAttribs.keys.elements ) {
                  lookup += "attributeName" -> aDecl;
                  n.children.elements.foreach{ n => writeNode( n ) }
                }
                lookup -= "attributeName";
              }
              case "ccstring" => {
                fOut.print( cookedCap( lookup( n("ref").get ) ));
              }
              case "cstring" => {
                fOut.print( cooked( lookup( n("ref").get ) ));
              }
              case "string" => {
                fOut.print( lookup( n("ref").get ) );
              }
              case "qstring" =>  {
                fOut.print("\"");
                fOut.print( lookup( n("ref").get ) );
                fOut.print("\"");
              }
              case _ => error("what shall I do with a \""+n.label+"\" node ?")
            }
        }
      }
      writeNode( tmpl )
    }
  }

  /** runs translation. */
  def run:Unit = {
    new objectTemplate {
      override val objectName = moduleName
    }.write;
    fOut.flush();
    fOut.close();
  }

  private def warning_attrib(name:String,tpe:String) = {
    System.err.print( "[ignoring attribute \"" );
    System.err.print( name );
    System.err.print( "\" of type \"" );
    System.err.print( tpe );
    System.err.print( "\"]" );
  }


//
// cooking raw names
//

/* replace dash, colons with underscore, keywords with appended $ */
private def cooked( ckd:StringBuffer, raw:String, off:int ):String = {
  for( val i <- List.range( off, raw.length()) ) {
    val _ = raw.charAt( i ).match {
      case '-' =>
        ckd.append( '_' )
      case ':' =>
        ckd.append( '_' )
      case x =>
        ckd.append( x )
    };
  };

  if( Tokens.isKeyword(raw)) {
    val _ = ckd.append('$');
  };
  ckd.toString()

}

// type       -> type$
// http-equiv -> http_equiv

private def cooked( raw:String ):String  = cooked(new StringBuffer(), raw, 0);


// type       -> Type$
// http-equiv -> Http_equiv

private def cookedCap( raw:String ):String = {
  val ckd:StringBuffer = new StringBuffer();
  ckd.append( Character.toUpperCase( raw.charAt( 0 ) ));
  cooked( ckd, raw, 1);
}

  Tokens; // initialize tokens

}

}
