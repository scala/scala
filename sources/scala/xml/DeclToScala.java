package scala.xml ;

import java.io.Writer ;
import java.io.PrintWriter ;
import java.io.OutputStream ;
import java.io.OutputStreamWriter ;

import java.util.Map ;
import java.util.Iterator ;

import scala.xml.XMLDecl ;
import XMLDecl.* ;

import scalac.util.Name ;
import scalac.ast.parser.Scanner ;


public class DeclToScala {


      static final String ATTRIBS_VARDEF =
	  "var _at:scala.xml.javaAdapter.Map[String,String] = new HashMap[String,String];";
      //static final String ATTRIB_MAP = "attribs";
      //static final String ATTRIB_T   = "scala.Map[String,String]";

      static final String CHILDREN_VALDEF =
"var _ch:scala.Seq[scala.xml.Element] = if( children == null ) { scala.Nil } else children ;";
      static final String CHILDREN_SEQ = "children";
    //static final String CHILDREN_T   = "scala.Seq[Element]";
      static final String CHILDREN_T   = "Element*";

      static final String RAW_NAME_DEF     = "def getName:String = ";

      static final String GET_CHILDREN_DEF = "def getChildren:scala.Seq[scala.xml.Element] = _ch ;";
      static final String SET_CHILDREN_DEF = "def setChildren( l:scala.Seq[scala.xml.Element] ):Unit = {_ch = l};";
      static final String GET_ATTRIBS_DEF =  "def getAttribs:scala.xml.javaAdapter.Map[String,String] = _at ;";
      static final String SET_ATTRIBS_DEF =  "def setAttribs( m:scala.xml.javaAdapter.Map[String,String] ):Unit = {_at = m};";

      static final int IND_STEP = 5;

      public DeclToScala() {

            fOut = new PrintWriter( System.out );
            this.moduleName = "myXML" ;

      }

      public DeclToScala( String moduleName ) {

            fOut = new PrintWriter( System.out );
            this.moduleName = moduleName ;
      }

      String moduleName ;

      PrintWriter fOut ;

      int fIndent ;

      public void begin() {
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

      public void end() {
            fOut.println("}");
            fOut.flush();
            fOut.close();
      }

      /** Sets the output stream for printing. */
      /*
      public void setOutput(OutputStream stream, String encoding)
            throws java.io.UnsupportedEncodingException {

            if (encoding == null) {
                  encoding = "UTF8";
            }

            Writer writer = new OutputStreamWriter(stream, encoding);
            fOut = new PrintWriter(writer);

      } // setOutput(OutputStream,String)
      */
      /**
       * @name - (raw) name of the Element
       */
      void printClassDef( ElemDecl decl ) {
            String clazzName = cookedCap( decl.name );

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
            fIndent += IND_STEP ;

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

	    /*
            for( Iterator it = decl.attribs.keySet().iterator(); it.hasNext() ; )
                  toScala( (AttrDecl) decl.attribs.get( it.next() ));
	    */

            fIndent -= IND_STEP ;
            printIndent();
            fOut.println("}");
            fOut.flush();
      }

      /** Prints the indent. */
      void printIndent() {
            for (int i = 0; i < fIndent; i++) {
                  fOut.print(' ');
            }
      }

      /**
       * @name - (raw) name of the attribute
       */
      void printSetMethod( String name ) {
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
      void printGetMethod( String name ) {
            printIndent();

            fOut.print( "def " );
            fOut.print( cooked( name ));
            fOut.print( ":String = _at.get( ");
            fOut.print( '"' );fOut.print( name );fOut.print( '"' );
            fOut.println( " ) ;");
      }

      void printFactory( Map elemMap ) {

            printIndent();
            fOut.println(
"val _factory: scala.xml.javaAdapter.Map[String, scala.Seq[scala.xml.Element] => scala.xml.Element] = {");
            fIndent += IND_STEP;
            printIndent();
            fOut.println(
			 //"val res = new scala.HashMap[String,(scala.Map[String,String],scala.Seq[Element])=>Element] ;");
"val res = new scala.xml.javaAdapter.HashMap[String, scala.Seq[scala.xml.Element] => scala.xml.Element] ;");
            for(Iterator it = elemMap.keySet().iterator(); it.hasNext(); ) {
                  ElemDecl decl = (ElemDecl) elemMap.get( it.next() );
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
            fIndent -= IND_STEP;

      }

      void printContainsTextDef( Map elemMap ) {
            printIndent();
            fOut.println("val _containsMap: scala.xml.javaAdapter.Map[scala.String, scala.Boolean] = {");
            fIndent += IND_STEP;
            printIndent();
            fOut.println("val res = new scala.xml.javaAdapter.HashMap[scala.String, scala.Boolean] ;");

            for(Iterator it = elemMap.keySet().iterator(); it.hasNext(); ) {
                  ElemDecl decl = (ElemDecl) elemMap.get( it.next() );
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
            fIndent -= IND_STEP;
            printIndent();
            fOut.println( "}");
      }

      /*
                                name match {
                                        case "groupname" => True;
                                        case "name" => True;
                                        case _      =>False
                                }
                        }
                }
      */

      void printLoaderDef( ) {
            printIndent();
            fOut.println("def load( filename:String ):Element = {");
            fIndent += IND_STEP;
            printIndent();
            fOut.println("val fAdapter = new ScalaFactoryAdapter  {");
            fIndent += IND_STEP;
            printIndent();
            fOut.println("val f = _factory ;");
            printIndent();
            fOut.println("val g = _containsMap ; ");
            printIndent();
            fOut.println("};");
            fIndent -= IND_STEP;
            printIndent();
            fOut.println("val b:scala.Object = fAdapter.loadXML( filename );");
            printIndent();
            fOut.println("b as Element");
            printIndent();
            fOut.println("};");
            fIndent -= IND_STEP;
      }

      public void toScala( Map elemMap ) {
            begin();

            fOut.println("/** the following elements are there");
            for(Iterator it = elemMap.keySet().iterator(); it.hasNext(); ) {

                  ElemDecl decl = (ElemDecl) elemMap.get( it.next() );
                  fOut.print(" * ");
                  fOut.print( decl.name );
                  fOut.print(" : [");
                  fOut.print( decl.contentModel );
                  fOut.println(']');
                  fOut.print(" * ");
                  fOut.println( "attribs: "+decl.attribs.keySet() );
            }
            fOut.println("*/");


            for(Iterator it = elemMap.keySet().iterator(); it.hasNext(); ) {

                  ElemDecl decl = (ElemDecl) elemMap.get( it.next() );
                  toScala( decl );

            }
            printContainsTextDef( elemMap );
            printFactory( elemMap );
            printLoaderDef();

            end();
      }

      public void toScala( XMLDecl decl ) {
            switch(decl) {
            case ElemDecl( _, _, _):

                  printClassDef( (ElemDecl) decl );
                  break;

            case AttrDecl( String name, String attrType ):

                  if( attrType.equals("CDATA") ) {
                        //printSetMethod(name);
                        printGetMethod(name);

                  } else { // ignore non-CDATA attribs

                        System.err.print("[ignoring attribute \""
                                           +name+"\" of type \""
                                           +attrType+"\"]");
                  }
                  break;
            default:
                  System.err.println("unexpected XMLDecl");
                  System.exit(-1);
            }
      }


      //
      // cooking raw names
      //

      String cooked(StringBuffer cooked, String raw, int off) {
            char ch;
            for( int i = off; i < raw.length(); i++ ) {
                  switch( ch = raw.charAt( i ) ) {
                  case '-':
                  case ':':
                        cooked.append( '_' );break;

                  default:
                        cooked.append( ch ); break;
                  }
            }

            if( toy.isKeyword(raw))
                  cooked.append('$');

            return cooked.toString();

      }

      // type       -> type$
      // http-equiv -> http_equiv

      String cooked(String raw) {
            return cooked(new StringBuffer(), raw, 0);
      }

      // type       -> Type$
      // http-equiv -> Http_equiv

      String cookedCap(String raw) {
            StringBuffer cooked = new StringBuffer();
            cooked.append( Character.toUpperCase( raw.charAt( 0 ) ));
            return cooked( cooked, raw, 1);
      }
    /*
      static class ToyScanner extends Scanner {
            public ToyScanner() {
                  initKeywords();
            }

            boolean isKeyword( String str ) {
                  Name name = Name.fromString( str );
                  return (name.index <= maxKey) ;
            }
      }
    */
      static Scanner toy = new Scanner();

}
