package scala.tools.dtd2scala ;
import org.xml.sax.{InputSource,SAXException,SAXParseException,XMLReader}
//import org.xml.sax.helpers.XMLReaderFactory;

import java.io.{File,FileWriter,PrintWriter,StringReader};

import javax.xml.parsers.SAXParserFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;

object Main {

  // included in JDK1.4, but cannot resolve relative sysIDs
  //final val DEFAULT_PARSER_NAME = "org.apache.crimson.parser.XMLReaderImpl" ;
  //final val DEFAULT_PARSER_NAME = "org.apache.xerces.parsers.SAXParser" ;

  /** Default namespaces support (true). */
  final val DEFAULT_NAMESPACES = false;

  final val DECL_HANDLER = "http://xml.org/sax/properties/declaration-handler";

  final val NAMESPACES_FEATURE_ID = "http://xml.org/sax/features/namespaces";


  def  printUsage:Unit = {
    Console.println("usage: dtd2scala [ -d <dir> ] <sysID> <object name> [<namespace>]");
    Console.println("       binds a DTD to class definitions");
    Console.println("       will create a file [<dir>/]<object name>.scala");
    Console.println("<dir> is the output directory [path of <sysID> is default]");
    Console.println("<sysID> is a system ID of an XML DTD");
    Console.println("<object name> is the name of the resulting Scala source file ");
  }

  /** translates dtd to scala class definitions. see also printUsage */
  def main(argv:Array[String]):Unit = {
    //import scala.Seq ; // to shadow case class Seq RegExp

    List.fromArray(argv, 0, argv.length) match {
      case Seq( "-d", outdir, sysID, objName ) =>
        continue( new File( outdir ), sysID, objName, "" );
      case Seq( "-d", outdir, sysID, objName, namespace ) =>
        continue( new File( outdir ), sysID, objName, namespace );
      //case Seq( "-sql", sysID ) => dosql( sysID );
      case Seq( sysID, objName ) =>
        continue( new File( sysID ).getParentFile(), sysID, objName, "" );
      case Seq( sysID, objName, namespace ) =>
        continue( new File( sysID ).getParentFile(), sysID, objName, namespace );
      case _ =>
        { printUsage; System.exit(-1); }
    }
  }

  private def continue( outdir:File, sysID:String, objName:String, ns:String ) = {
    val myH:MainHandler  = new MainHandler();
    parse( sysID, myH );
    // myH.print(); // DEBUG

    val p = new PrintWriter(new FileWriter(new File(outdir,
                                                    objName+".scala" )));
    new DeclToScala( p, objName, ns, myH.elemMap ).run;
  }

  /*
  private def dosql( sysID:String ) = {
    val myH:MainHandler  = new MainHandler();
    parse( sysID, myH );
    //val q = new PrintWriter(new FileWriter(new File(outdir,
    //                                                objName+".sql" )));
    val q = new PrintWriter(System.out);
    new DeclToSQL( q, null:String, myH.elemMap ).run;
  }
*/

  private def inputsrc( sysID:String ):InputSource = {
    // create isrc for a fake doc referencing the DTD
    val  isrc:InputSource = new InputSource(
      new StringReader("<!DOCTYPE doc SYSTEM \""+ sysID +"\"><doc></doc>")
    );
    val curDir:String = System.getProperty("user.dir");
    isrc.setSystemId( "file:///"+curDir+"/fubar.xml" );
    isrc;
  }

  private def parse( sysID:String, myH:MainHandler ) = {
    val parser:SAXParser  = SAXParserFactory.newInstance().newSAXParser();

    /*
    try {parser.setFeature( NAMESPACES_FEATURE_ID,
                            DEFAULT_NAMESPACES );}
    catch {
      case e:SAXException =>
    }
*/
    try   { parser.setProperty( DECL_HANDLER, myH ); }
    catch { case e:SAXException => e.printStackTrace(System.err); }
    try { parser.parse( inputsrc( sysID ), myH ); }
    catch {
      case e:SAXParseException =>
        System.err.println("SaxParseEx");e.printStackTrace( System.err );
      case e:Exception => {
        System.err.println("error: Parse error occurred - "+e.getMessage());
        if( e.isInstanceOf[SAXException] )
          e.asInstanceOf[SAXException].getException()
        .printStackTrace( System.err )
        else e.printStackTrace()
      }
    }

  }

} //object main
