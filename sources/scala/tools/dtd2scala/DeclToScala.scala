// $Id$

import scalac.util.Name ;
import scala.tools.scalac.ast.parser.Tokens ; /* for keywords */

package scala.tools.dtd2scala {

import java.io.PrintWriter ;
import org.xml.sax.InputSource ;

import scala.collection.Map ;
import scala.collection.mutable.HashMap ;

import scala.xml._ ;
import scala.xml.dtd.{AttrDecl, ContentModel, ElemName, ElemDecl};

import ContentModel._ ;
import scala.xml.dtd.{REQUIRED,IMPLIED,DEFAULT};
import scala.xml.nobinding.XML ;

/** transforms a set of DTD declaraion to a scala source file.
 *  2do: parameterize with destination package.
 */
class DeclToScala(fOut: PrintWriter, objectName: String, namespace: String, elemMap:Map[String, MyElemDecl] ) {

  class ObjectTemplate {
    val package_ : String = "";
    val compress : boolean = true;
    final val tmplFile = "scala/tools/dtd2scala/template/ObjectTemplate.scala.xml";
    final val tmpl:Node = XML.load( new InputSource(ClassLoader.getSystemResourceAsStream(tmplFile)) );

    val lookup : HashMap[String,String] = new HashMap[String,String]();
    var curAttribs: Map[String,AttrDecl] = null ;  /* of current elem */
    var curModel: RegExp = null;

    /** 1.populate with special "<R" and default
    **/
    def initAttributes( curAttribs:Map[String,AttrDecl] ):String = {
        val sb = new StringBuffer();
        for( val key <- curAttribs.keys ) {
          curAttribs( key ).default match {
            case REQUIRED =>
              sb.append("map = map.update(\"");
              sb.append( key );
              sb.append("\", ");
              sb.append("\"<R\"); req=req+1; // REQUIRED");
            case IMPLIED  =>
              /* no default value */
            case DEFAULT( _, attValue ) =>
              sb.append("map = map.update(\"");
              sb.append( key );
              sb.append("\", \"");
              sb.append(attValue); /* quotes??? */
              sb.append("\");");
          }
          sb.append('\n');
        }
        sb.toString();
    }
    /** 1.populate with actual values: throw error if FIXED is wrong
    **  2.throw error if a REQUIRED one is missing*/
    def validateAttributes(curAttribs: Map[String,AttrDecl]):String = {
      def appendKey( key:String, sb:StringBuffer ) = {
        val it = Iterator.fromString( key );
        sb.append('\'');
        sb.append( it.next );
        sb.append('\'');
        it.foreach {
          c =>
          sb.append(',');
          sb.append('\'');
          sb.append( c );
          sb.append('\'')
        }
      }
      val sb = new StringBuffer();
      for( val key <- curAttribs.keys ) {
        sb.append(" case Seq((),");
        curAttribs( key ) match {
          case AttrDecl( key, tpe, df ) =>
          appendKey( key, sb );
          sb.append(") =>");
          df match {
            case DEFAULT( true, attValue ) =>
              sb.append("if( b._2 != \"");
              sb.append( attValue );
              sb.append("\" ) error_FixedAttribute(b._1,\"");
              sb.append( attValue );
              sb.append("\") else {};");
            case REQUIRED =>
              sb.append(" req = req - 1; map = map.update( b._1, b._2 );");
            case _ =>
              sb.append(" map = map.update( b._1, b._2 );");
          }
        }
        sb.append('\n');
      }
      sb.toString();
    }

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
      /*Console.println("sCM:"+ r.getClass() + " r:"+r );*/
      r match {
        case Eps   => ""
        case Letter(ElemName(name)) => refTag( name ).toString();
        case Star(r) => "("+shallowContentModel( r ) +") *"
        case Alt( rs @ _* )  => shallowContentModel1( rs, '|' )
        case Sequ( rs @ _* ) =>  shallowContentModel1( rs, ',' )
        case _ => error("unexpected regexp:"+r);
      }
    }

      /*Console.println("shallowValid:"+ r.getClass() + " r:"+r );*/
      r match {
      case ANY_ => "true";
      case Eps  => "ch.length == 0"
      case PCDATA_ | Sequ( PCDATA_ ) =>
        val sb = new StringBuffer("tagIterator( ch ).forall( x => x < 0 )");
        sb.toString();
      case Star(Alt(PCDATA_, alts @ _*)) => {
        val sb = new StringBuffer("tagIterator( ch ).forall( x:Int => x match {");
        for( val alt <- alts.elements ) {
          sb.append("                          case ");
          alt match { case Letter(ElemName( name )) => sb.append( refTag( name ).toString() )}
          sb.append(" => true \n");
        }
        sb.append("case _ => false })");
        sb.toString();
      }
      case _ =>  val sb = new StringBuffer("tagIterator( ch ).toList.match {");
        sb.append("         case Seq(");
        sb.append( shallowContentModel( r ) );
        sb.append( ") => true \n");
        sb.append( "case _ => Console.println( tagIterator( ch ).toList);false }\n");
        sb.toString();
      }
    }

    var tagCounter = 0;
    val refTag = new HashMap[String,Int]();
    def newTag(elemName:String) = {
      val i = tagCounter;
      tagCounter = tagCounter + 1;
      refTag.update( elemName, i );
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
              case "attributeDecls" =>
                var sb = new StringBuffer("Nil");
                for( val aDecl <- curAttribs.values ) {
                  sb.insert(0, "::");
                  sb.insert(0, aDecl.toString());
                }
                fOut.print(sb.toString());

              case "elemDecl" =>
                fOut.print(ContentModel.toString( curModel ));

              case "template" => {
                lookup.update("objectName", objectName);
                lookup.update("namespace",  namespace);
                lookup.update("compressDefault", compress.toString());
                n.child.elements.foreach { n => writeNode(n) }
              }
              case "initAttributes" =>
                fOut.print(  initAttributes( curAttribs ) );
              case "validateAttributes" =>
                fOut.print(  validateAttributes( curAttribs ) );
              case "shallowContentRegExp" =>
                fOut.print(  shallowValidate( curModel ) );
              case "makeTag" =>
                fOut.print( newTag( lookup("elementName") ).toString() );
              case "refTag" =>
                fOut.print( refTag( lookup("elementName") ).toString() );
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
                  lookup += "attributeDecl" -> curAttribs(aDecl).toString();
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
    new ObjectTemplate().write;
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
