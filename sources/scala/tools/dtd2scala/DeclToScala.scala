import scalac.util.Name ;
import scala.tools.scalac.ast.parser.Tokens ; /* for keywords */

package scala.tools.dtd2scala {

import java.io.PrintWriter ;
import org.xml.sax.InputSource ;

import scala.collection.Map ;
import scala.collection.mutable.HashMap ;

import scala.xml._ ;
import scala.xml.dtd.{AttrDecl,RegExp,ANY_,PCDATA_,Eps,Star,RNode,Sequ,Alt};
import scala.xml.nobinding.XML ;

/** transforms a set of DTD declaraion to a scala source file.
 *  2do: parameterize with destination package.
 */
class DeclToScala(fOut:PrintWriter,
		  moduleName:String,
		  elemMap:Map[ String, MyElemDecl ] ) {

  abstract class objectTemplate {
    val objectName : String = "myXML"; /* DEFAULT MODULE NAME */
    val package_ : String = "";
    val compress : boolean = true;
    final val tmplFile = "scala/tools/dtd2scala/template/ObjectTemplate.scala.xml";
    final val tmpl:Node = XML.load( new InputSource(ClassLoader.getSystemResourceAsStream(tmplFile)) );

    val lookup : HashMap[String,String] = new HashMap[String,String]();
    var curAttribs: Map[String,AttrDecl] = null ;  /* of current elem */
    var curModel : RegExp = null;

    def shallowValidate( r:RegExp ):String = {

    def shallowContentModel1(rs:List[RegExp],sep:Char):String = {
      val it = rs.elements;
      val sb = new StringBuffer();
      sb.append('(');
      sb.append(  shallowContentModel( it.next ) );
      for( val z <- it ) {
        sb.append( sep );
        sb.append( shallowContentModel( z ) );
      }
      sb.append( ')' );
      sb.toString()
    }

    def shallowContentModel(r:RegExp):String = {
      //Console.println("sCM:"+ r.getClass() + " r:"+r );
      r match {
        case Eps   => ""
        case RNode(name) => "$TagOf" + cookedCap( name );
        case Star(r) => "("+shallowContentModel( r ) +") *"
        case Alt( rs @ _* )  => shallowContentModel1( rs, '|' )
        case Sequ( rs @ _* ) =>  shallowContentModel1( rs, ',' )
        case _ => error("unexpected regexp:"+r);
      }
    }

      //Console.println("shallowValid:"+ r.getClass() + " r:"+r );
      r match {
      case ANY_ => "true";
      case Eps  => "ch.length == 0"
      case PCDATA_ | Sequ( PCDATA_ ) =>
        val sb = new StringBuffer("ch.elements.forall( x:scala.xml.Node => x match {\n");
        sb.append("                          case _:scala.xml.Text      => true \n");
        sb.append("                          case _:scala.xml.EntityRef => true \n");
        sb.append("                          case _:scala.xml.CharData  => true \n");
        sb.append("                          case _:scala.xml.Comment   => true \n");
        sb.append("                          case _:scala.xml.ProcInstr  => true \n");
        sb.append("case _ => false })");
        sb.toString();
      case Star(Alt(PCDATA_, alts @ _*)) => {
        val sb = new StringBuffer("ch.elements.forall( x:scala.xml.Node => x match {\n");
        sb.append("                          case _:scala.xml.Text      => true \n");
        sb.append("                          case _:scala.xml.EntityRef => true \n");
        sb.append("                          case _:scala.xml.CharData  => true \n");
        sb.append("                          case _:scala.xml.Comment   => true \n");
        sb.append("                          case _:scala.xml.ProcInstr  => true \n");
        // classes created with mixin composition... types won't work
        sb.append("                          case _ => x.typeTag$ match {\n");
        // no type tag -> is scala.xml.Elem
        sb.append("                            case 0  => x.label match {");
        for( val alt <- alts.elements ) {
          sb.append("                          case \"");
          alt match { case RNode( name ) => sb.append( name );}
          sb.append("\" => true \n");
        }
        sb.append("case _ =>  Console.println(\"ELEM\"); for( val z <- ch ) { Console.println( z.getClass() ); Console.println( z.label ); Console.println( z.toString() )}; false}\n");
        for( val alt <- alts.elements ) {
          sb.append("                          case $TagOf");
          alt match { case RNode( name ) => sb.append( cookedCap( name ));}
          sb.append(" => true \n");
        }
        sb.append("case _ => Console.println(ch.toList); for( val z <- ch ) { Console.println( z.getClass() ); Console.println( z.label ); Console.println( z.toString() )};  false }})");
        sb.toString();
      }
        case _ =>  val sb = new StringBuffer("(ch.elements.foldLeft (Nil:List[Int]) { (e:List[Int],x:scala.xml.Node) => val i = x.typeTag$; if( i!=0 ) i::e else e }).reverse match {");
        sb.append("         case Seq( ");
        sb.append( shallowContentModel( r ) );
        sb.append( ") => true ");
        sb.append( "case _ => Console.println(\"reg match fails for \"+ch.elements.foldLeft (Nil:List[Int]) { (e:List[Int],x:scala.xml.Node) => val i = x.typeTag$; if( i!=0 ) i::e else e }); false}");
        sb.toString();
      }
    }

    var tagCounter = 0;
    def newTag = {
      val i = tagCounter;
      tagCounter = tagCounter + 1;
      i
    }
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
                n.child.elements.foreach { n => writeNode(n) }
              }
              case "shallowContentRegExp" =>
                fOut.print(  shallowValidate( curModel ) );
              case "elementTag" =>
                fOut.print( newTag.toString() );
              case "elementBinding" => {
                for( val decl <- elemMap.values ) {
                  lookup += "elementName" -> decl.name;
                  lookup += "elementContainsText" -> decl.containsText.toString();
                  lookup += "elementContentModel" -> decl.contentModel;
                  //Console.println("elemName="+decl.name);
                  curModel = decl.parsedContentModel ;
                  //Console.println("curModel="+curModel);
                  curAttribs = decl.attribs;
                  n.child.elements.foreach{ n => writeNode( n ) }
                }
                curModel = null;
                curAttribs = null;
                lookup -= "elementName";
                lookup -= "elementContainsText";
              }
              /*
              case "attributeAssign" => {
                for( val aDecl <- curAttribs.keys.elements ) {
                  lookup += "attributeName" -> aDecl;
                  n.child.elements.foreach{ n => writeNode( n ) }
                }
                lookup -= "attributeName";
              }
              */
              case "attributeBinding" => {
                for( val aDecl <- curAttribs.keys ) {
                  lookup += "attributeName" -> aDecl;
                  n.child.elements.foreach{ n => writeNode( n ) }
                }
                lookup -= "attributeName";
              }
              case "ccstring" => {
                fOut.print( cookedCap( lookup( n.attribute("ref") ) ));
              }
              case "cstring" => {
                fOut.print( cooked( lookup( n.attribute("ref") ) ));
              }
              case "string" => {
                fOut.print( lookup( n.attribute("ref") ) );
              }
              case "qstring" =>  {
                fOut.print("\"");
                fOut.print( lookup( n.attribute("ref") ) );
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
