package scala.tools.dtd2scala ;

import org.xml.sax.SAXParseException;
import org.xml.sax.SAXException;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.Attributes;
import org.xml.sax.ext.DeclHandler;

import java.io.{File,FileWriter,PrintWriter,StringReader};

import java.util.Map ;
import java.util.HashMap ;
import java.util.TreeMap ;
import java.util.Iterator ;

object Main {
    // this implementation included in JDK1.4, but cannot resolve relative sysIDs
    final val DEFAULT_PARSER_NAME:String = "org.apache.crimson.parser.XMLReaderImpl" ;

    //	DEFAULT_PARSER_NAME = "org.apache.xerces.parsers.SAXParser" ;

    final val DOC_WRAPPER_FRONT:String = "<!DOCTYPE doc SYSTEM \"";
    final val DOC_WRAPPER_BACK:String = "\"><doc></doc>";

  /** print usage string
  */
    def  usage():Unit = {
	System.out.println("usage: dtd2scala [ -d <dir> ] <sysID> <object name>");
	System.out.println("       binds a DTD to class definitions");
	System.out.println("       will create a file [<dir>/]<object name>.scala");
	System.out.println("<dir> is the output directory [default is the path of <sysID>]");
	System.out.println("<sysID> is a system ID of an XML DTD");
	System.out.println("<object name> is the name of the resulting Scala source file ");

    }

    /** main method. parses dtd of doc with <sysID> and translates to scala case
     * class definitions.
     *  definitions are printed to stdout.
     *  @argv two parameters, sysID and object name
     */

    def main(argv:Array[String]):Unit /*throws Exception*/ = {

	if ( argv.length == 0 ) {
	    usage();
	    System.exit(-1);
	}

	if(( argv.length != 2 )
	   &&(( argv.length != 4 )
	      ||(( argv.length == 4 )&&( !argv( 0 ).equals("-d"))))) {
	    //System.out.println( argv.length+"ga"+argv[0].equals("-d") );
	    usage();
	    System.exit(-1);
	}

	val myH:MainHandler  = new MainHandler();
	val sysID:String  = argv( argv.length - 2  );
	val objname:String  = argv( argv.length - 1 );
	var outdir:File = null.asInstanceOf[ File ];
	if ( argv.length == 4 ) {
	    outdir = new File( argv( 1 ));
	} else {
	    outdir = new File( sysID ).getParentFile();
	}
	val parser:XMLReader  =
	    XMLReaderFactory.createXMLReader( DEFAULT_PARSER_NAME );

	try {
	    parser.setProperty("http://xml.org/sax/properties/declaration-handler", myH);
	}
	catch{
	  case  e:SAXException =>
	    e.printStackTrace(System.err);
	}
	try {
	  // create isrc for a fake doc referencing the DTD
	  val  isrc:InputSource = new InputSource(
	    new StringReader( DOC_WRAPPER_FRONT
			     + sysID
			     + DOC_WRAPPER_BACK ) );
	  val curDir:String = System.getProperty("user.dir");
	  //String sysID2 =  new File( sysID ).getAbsolutePath();
	  //System.err.println( "file:///"+curDir+"/fubar.xml" );
	  isrc.setSystemId( "file:///"+curDir+"/fubar.xml" );
	  parser.parse( isrc );
	}
	catch {
           case e:SAXParseException =>
	     System.err.println("SaxParseEx");
	     e.printStackTrace( System.err );
	  case e:Exception =>
	    System.err.println("error: Parse error occurred - "+e.getMessage());
	    if( e.isInstanceOf[SAXException] ) {
	      e.asInstanceOf[SAXException].getException().printStackTrace(System.err)
	    } else {
  	      e.printStackTrace()
	    }
	}

	// myH.print(); // DEBUG

	// instantiate translator
	/* does not work due to backend handling of constructors, FIXME
           val translate:DeclToScala = new DeclToScala( outdir, objname );
	*/
        val p = new PrintWriter( new FileWriter( new File( outdir, objname+".scala" )));
        val translate:DeclToScala = new DeclToScala( p, objname );
	// translate
	translate.toScala( myH.elemMap );


    } // main
} //object main

