package scala.tools.dtd2scala ;

import java.io.Writer ;
import java.io.PrintWriter ;
import java.io.File ;
import java.io.FileWriter ;
import java.io.OutputStream ;
import java.io.OutputStreamWriter ;

import java.io.IOException ;

import java.util.Map ;
import java.util.Iterator ;

import scalac.util.Name ;
import scalac.ast.parser.Scanner ;

class DeclToScala(fOut:PrintWriter, moduleName:String) {

  val DEFAULT_moduleName = "myXML";

  final val COMPRESS_DEFAULT:String  = "true"; // do hash-consing on load

  final val ATTRIBS_VARDEF:String  =
    "var _at:scala.xml.javaAdapter.Map[String,String] = new HashMap[String,String];";
      //static final String ATTRIB_MAP = "attribs";
      //static final String ATTRIB_T   = "scala.Map[String,String]";

  final val CHILDREN_VALDEF:String  =
    "var _ch:scala.Seq[scala.xml.Element] = if( children == null ) { scala.Nil } else children ;";
  final val CHILDREN_SEQ:String  = "children";
  //static final String CHILDREN_T   = "scala.Seq[Element]";
 final val CHILDREN_T:String    = "Element*";

  final val RAW_NAME_DEF:String      = "def getName:String = ";

  final val GET_CHILDREN_DEF:String  = "def getChildren:scala.Seq[scala.xml.Element] = _ch ;";
  final val SET_CHILDREN_DEF:String  = "def setChildren( l:scala.Seq[scala.xml.Element] ):Unit = {_ch = l};";
  final val GET_ATTRIBS_DEF:String  =  "def getAttribs:scala.xml.javaAdapter.Map[String,String] = _at ;";
  final val SET_ATTRIBS_DEF:String  =  "def setAttribs( m:scala.xml.javaAdapter.Map[String,String] ):Unit = {_at = m};";

  //static final String HASHCODE_DEF =  "override def hashCode():int = { getChildren.hashCode() + getAttribs.hashCode() + getName.hashCode() }";

  final val IND_STEP:int  = 5;
/*
  def this() = { this(new PrintWriter( System.out ), DEFAULT_moduleName) }

  def this( outdir:File, moduleName:String ) = { this(
            new PrintWriter( new FileWriter( new File( outdir, moduleName+".scala" ))),
	    moduleName) }
*/
  var fIndent:int = 0;

  def begin():Unit = {
    fOut.println( "// this file is generated from a DTD");
    fOut.print( "object " );
    fOut.print( moduleName );
    fOut.println(" {");
    fIndent = IND_STEP;
    printIndent();
    fOut.println("import scala.xml._ ;");
    fOut.println("import scala.xml.javaAdapter.Map ;");
    fOut.println("import scala.xml.javaAdapter.HashMap ;");
  }

  def end():Unit = {
    fOut.println("}");
    fOut.flush();
    fOut.close();
  }

  /**
  * @name - (raw) name of the Element
  */
  def printClassDef( decl:ElemDecl ):Unit = {
    val clazzName:String = cookedCap( decl.name );

    printIndent();

    /* // DISABLED
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


    fOut.print( "case class " );

    fOut.print( clazzName );

    fOut.print('(');
               //fOut.print( ATTRIB_MAP ); fOut.print(':'); fOut.print( ATTRIB_T );
		 //fOut.print(',');
               fOut.print( CHILDREN_SEQ ); fOut.print(':'); fOut.print( CHILDREN_T );
               fOut.print(')');

    fOut.println(" extends Element {");
    fIndent = fIndent + IND_STEP ;

    printIndent();
    fOut.print( RAW_NAME_DEF );
    fOut.print('"'); fOut.print( decl.name ); fOut.print('"');
    fOut.println(';');

    printIndent(); fOut.println( CHILDREN_VALDEF );
    printIndent(); fOut.println( GET_CHILDREN_DEF );
    printIndent(); fOut.println( SET_CHILDREN_DEF );
    printIndent(); fOut.println( ATTRIBS_VARDEF );
    printIndent(); fOut.println( GET_ATTRIBS_DEF );
    printIndent(); fOut.println( SET_ATTRIBS_DEF );
    //printIndent(); fOut.println( HASHCODE_DEF );

    /*
    for( Iterator it = decl.attribs.keySet().iterator(); it.hasNext() ; )
      toScala( (AttrDecl) decl.attribs.get( it.next() ));
    */

    fIndent = fIndent - IND_STEP ;
    printIndent();
    fOut.println("}");
    fOut.flush();
  }

  /** Prints the indent. */
  def printIndent():Unit = {
    for (val i<-List.range(0, fIndent)) do {
      fOut.print(' ');
    }
  }

  /**
  * @name - (raw) name of the attribute
  */
  def printSetMethod( name:String ):Unit = {
    printIndent();

    fOut.print( "def " );
    fOut.print( cooked( name ));
    fOut.print( "( arg:String ):Unit = _at.put(\"" );
               fOut.print( name );
               fOut.println( "\", arg ) ;");
  }

  /**
  * @name - (raw) name of the attribute
  */
  def printGetMethod( name:String ):Unit = {
    printIndent();

    fOut.print( "def " );
    fOut.print( cooked( name ));
    fOut.print( ":String = _at.get( ");
    fOut.print( '"' );fOut.print( name );fOut.print( '"' );
    fOut.println( " ) ;");
  }

  def printFactory( elemMap:Map ):Unit = {

    printIndent();
    fOut.println(
      "val _factory: scala.xml.javaAdapter.Map[String, scala.Seq[scala.xml.Element] => scala.xml.Element] = {");
    fIndent = fIndent + IND_STEP;
    printIndent();
    fOut.println(
      //"val res = new scala.HashMap[String,(scala.Map[String,String],scala.Seq[Element])=>Element] ;");
      "val res = new scala.xml.javaAdapter.HashMap[String, scala.Seq[scala.xml.Element] => scala.xml.Element] ;");
    //JAVA: for(Iterator it = elemMap.keySet().iterator(); it.hasNext(); )
    val it:Iterator = elemMap.keySet().iterator();
    while( it.hasNext() ) {
      val decl:ElemDecl = elemMap.get( it.next() ).asInstanceOf[ ElemDecl ];
      printIndent();
      fOut.print( "res.put(\"" );
                 fOut.print( decl.name );
                 fOut.print( "\",(x:scala.Seq[scala.xml.Element] => { val res = ");
      fOut.print( cookedCap( decl.name ));
      fOut.println("(); res.setChildren(x); res }));");
    }
    printIndent();
    fOut.println("res");
    printIndent();
    fOut.println( "}");
    fIndent = fIndent - IND_STEP;

  }

  def printContainsTextDef( elemMap:Map ):Unit = {
    printIndent();
    fOut.println("val _containsMap: scala.xml.javaAdapter.Map[scala.String, boolean] = {");
    fIndent = fIndent + IND_STEP;
    printIndent();
    fOut.println("val res = new scala.xml.javaAdapter.HashMap[scala.String, boolean] ;");

    val it:Iterator = elemMap.keySet().iterator();
    while( it.hasNext() ) {
      val decl:ElemDecl = elemMap.get( it.next() ).asInstanceOf[ ElemDecl ];
      printIndent();
      fOut.print( "res.put(\"" );
                 fOut.print( decl.name );
                 fOut.print( "\",");

      if( decl.contentModel.indexOf("#PCDATA") != -1 )
        fOut.print("true");
      else
        fOut.print("false");

      fOut.println(");");
    }
    printIndent();
    fOut.println("res");
    fIndent = fIndent - IND_STEP;
    printIndent();
    fOut.println( "}");
  }

  def printLoaderDef():Unit = {
    printIndent();
    fOut.print("def load( filename:String ):Element = load( filename, ");
    fOut.print( COMPRESS_DEFAULT );
    fOut.println(");");
    printIndent();
    fOut.println("def load( filename:String, _compress:boolean ):Element = {");
    fIndent = fIndent + IND_STEP;
    printIndent();
    fOut.println("val fAdapter = new ScalaFactoryAdapter  {");
    fIndent = fIndent + IND_STEP;
    printIndent();
    fOut.println("val f = _factory ;");
    printIndent();
    fOut.println("val g = _containsMap ; ");
    printIndent();
    fOut.println("val compress = _compress ; ");
    printIndent();
    fOut.println("};");
    fIndent = fIndent - IND_STEP;
    printIndent();
    fOut.println("val b:scala.Object = fAdapter.loadXML( filename );");
    printIndent();
    fOut.println("b.asInstanceOf[Element]");
    printIndent();
    fOut.println("};");
    fIndent = fIndent - IND_STEP;
  };

  def toScala( elemMap:Map ):Unit = {
    begin();

    fOut.println("/** the following elements are there");

    val it:Iterator = elemMap.keySet().iterator();
    while( it.hasNext() ) {
      val decl:ElemDecl =  elemMap.get( it.next() ).asInstanceOf[ ElemDecl ];
      fOut.print(" * ");
      fOut.print( decl.name );
      fOut.print(" : [");
      fOut.print( decl.contentModel );
      fOut.println(']');
      fOut.print(" * ");
      fOut.println( "attribs: "+decl.attribs.keySet() );
    }
    fOut.println("*/");

    val it2:Iterator =  elemMap.keySet().iterator();
    while( it2.hasNext() ) {
      val decl:ElemDecl = elemMap.get( it2.next() ).asInstanceOf[ ElemDecl ];
      toScala( decl );
    }

    printContainsTextDef( elemMap );
    printFactory( elemMap );
    printLoaderDef();

    end();
  }

  def toScala( decl:XMLDecl ):Unit = {
  decl.match {
    case ElemDecl( _, _, _) =>

      printClassDef( decl.asInstanceOf[ ElemDecl ] );

    case AttrDecl( name, attrType ) =>

      if( attrType.equals("CDATA") ) {
        //printSetMethod(name);
        printGetMethod(name);

      } else { // ignore non-CDATA attribs

        System.err.print("[ignoring attribute \""
                         +name+"\" of type \""
                         +attrType+"\"]");
      }
    case _ =>
      System.err.println("unexpected XMLDecl"); System.exit(-1);
  }
}


//
// cooking raw names
//

def cooked( ckd:StringBuffer, raw:String, off:int ):String = {
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

def cooked( raw:String ):String  = {
  return cooked(new StringBuffer(), raw, 0);
}

// type       -> Type$
// http-equiv -> Http_equiv

def cookedCap( raw:String ):String = {
  val ckd:StringBuffer = new StringBuffer();
  ckd.append( Character.toUpperCase( raw.charAt( 0 ) ));
  return cooked( ckd, raw, 1);
}

  val toy:Scanner = new Scanner()

}
