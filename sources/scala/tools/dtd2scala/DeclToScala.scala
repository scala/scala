package scala.tools.dtd2scala ;

import java.io.PrintWriter ;

import scala.collection.Map ;
import scala.collection.mutable.HashMap ;
import scalac.util.Name ;
import scalac.ast.parser.Scanner ; /* for keywords */

import scala.xml._ ;
import scala.xml.nobinding.{Element,XML} ;

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
    final val tmpl:Node = XML.load( ClassLoader.getSystemResource(tmplFile) );

    val lookup : HashMap[String,String] = new HashMap[String,String]();
    var curAttribs: Map[String,AttrDecl] = null ;  /* of current elem */

    def write:Unit = {
      def writeNode( n:Node ):Unit = {
        n.label match {
          case "template" => {
            lookup.update("objectName", objectName);
            lookup.update("compressDefault", compress.toString());
            n.children.elements.foreach { n => writeNode(n) }
          }
          case "elementBinding" => {
            for( val decl <- elemMap.values.elements ) do {
              fOut.println();
              printIndent();
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

          case "attributeAssign" => {
            for( val aDecl <- curAttribs.keys.elements ) do {
              lookup += "attributeName" -> aDecl;
              n.children.elements.foreach{ n => writeNode( n ) }
            }
            lookup -= "attributeName";
          }

          case "attributeBinding" => {
            for( val aDecl <- curAttribs.keys.elements ) do {
              lookup += "attributeName" -> aDecl;
              Console.println("attributeName is "+aDecl+" = "+lookup("attributeName"));
              n.children.elements.foreach{ n => writeNode( n ) }
            }
            lookup -= "attributeName";
          }
          case "ccstring" => {
              Console.println("ccstring ref=\""+n.attributes("ref")+"\"");
            fOut.print( cookedCap( lookup( n.attributes("ref") ) ));
          }
          case "string" => {
              Console.println("string ref=\""+n.attributes("ref")+"\"");
            fOut.print( lookup( n.attributes("ref") ) );
          }
          case "qstring" =>  {
              Console.println("qstring ref=\""+n.attributes("ref")+"\"");

            fOut.print("\"");
            fOut.print( lookup( n.attributes("ref") ) );
            fOut.print("\"");
          }
          case "br" => { fOut.println(); printIndent() }
          case "inc" => fIndent = fIndent + IND_STEP
          case "dec" => fIndent = fIndent - IND_STEP
          case "#PCDATA" => fOut.print( n.asInstanceOf[Text].text )
          case _ => error("what shall I do with a \""+n.label+"\" node ?")
        }
      }
      writeNode( tmpl )
    }
  }

  final val COMPRESS_DEFAULT:String  = "true"; // do hash-consing on load

  final val ATTRIBS_VARDEF:String  =
    "var _at:Map[String,String] = new HashMap[String,String];";
      //static final String ATTRIB_MAP = "attribs";
      //static final String ATTRIB_T   = "scala.Map[String,String]";

  final val CHILDREN_VALDEF:String  =
    "var _ch:scala.Seq[scala.xml.Element] = if( children == null ) { scala.Nil } else children ;";
  final val CHILDREN_SEQ:String  = "ch";
  //static final String CHILDREN_T   = "scala.Seq[Element]";
 final val CHILDREN_T:String    = "Element*";

  final val RAW_NAME_DEF:String      = "def label:String = ";

  final val GET_CHILDREN_DEF:String  = "def children:scala.Seq[scala.xml.Element] = ch ;";
  //final val GET_ATTRIBS_DEF:String  =  "def getAttribs:Map[String,String] = _at ;";
  //final val SET_ATTRIBS_DEF:String  =  "def setAttribs( m:Map[String,String] ):Unit = {_at = m};";

  //static final String HASHCODE_DEF =  "override def hashCode():int = { getChildren.hashCode() + getAttribs.hashCode() + getName.hashCode() }";

  final val IND_STEP:int  = 5;

  var fIndent:int = 0;

  /*

    // convenience ! overloaded constructors, have to appear *before*
    // the class def and need the "final" modifier

    fOut.println( "final def "+clazzName+"(ch:Seq[Element]):"+clazzName+" = new "+clazzName+"( null[scala.Map[String,String]], ch ) ;" );

    printIndent();

    fOut.println( "final def "+clazzName+"( el:Element ):"+clazzName+" = new "+clazzName+"( null[scala.Map[String,String]], el::Nil[Element] ) ;" );

    printIndent();

    // might contain text
    if( decl.contentModel.indexOf("#PCDATA") != -1 ) {

      fOut.println( "final def "+clazzName+"( text:String ):"+clazzName+" = new "+clazzName+"( PCDATA( text ) );" );
    printIndent();

    }

    */


  /** Prints the indent. */
  def printIndent():Unit = {
    for (val i<-List.range(0, fIndent)) do {
      fOut.print(' ');
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
  for( val i <- List.range( off, raw.length()) ) do {
    val _ = raw.charAt( i ).match {
      case '-' =>
        ckd.append( '_' )
      case ':' =>
        ckd.append( '_' )
      case x =>
        ckd.append( x )
    };
  };

  if( toy.isKeyword(raw)) {
    val _ = ckd.append('$');
  };
  ckd.toString()

}

// type       -> type$
// http-equiv -> http_equiv

private def cooked( raw:String ):String  = {
  return cooked(new StringBuffer(), raw, 0);
}

// type       -> Type$
// http-equiv -> Http_equiv

private def cookedCap( raw:String ):String = {
  val ckd:StringBuffer = new StringBuffer();
  ckd.append( Character.toUpperCase( raw.charAt( 0 ) ));
  return cooked( ckd, raw, 1);
}

  private val toy:Scanner = new Scanner()

}
