/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml;

import Predef._
import scala.xml.parsing.NoBindingFactoryAdapter ;
import org.xml.sax.InputSource;

/** constants, and functions to load and save XML elements. use this when data binding is not
**  desired, i.e. when XML is handled using Symbol nodes
**/
object XML  {

  val xml       = "xml"
  val xmlns     = "xmlns"
  val namespace = "http://www.w3.org/XML/1998/namespace"
  val preserve  = "preserve";
  val space     = "space";
  val lang      = "lang";

  import java.io._ ;

  // functions for generic xml loading, saving

  /** loads XML from given file */
  final def loadFile( file: File ): scala.xml.Elem =
    new NoBindingFactoryAdapter().loadXML( new InputSource(
      new FileInputStream( file )
    ));

  /** loads XML from given file descriptor */
  final def loadFile( fileDesc: FileDescriptor ): scala.xml.Elem =
    new NoBindingFactoryAdapter().loadXML( new InputSource(
      new FileInputStream( fileDesc )
    ));

  /** loads XML from given file */
  final def loadFile( fileName:String ): scala.xml.Elem =
    new NoBindingFactoryAdapter().loadXML( new InputSource(
      new FileInputStream( fileName )
    ));

  /** loads XML from given InputStream */
  final def load( is:InputStream ): scala.xml.Elem =
    new NoBindingFactoryAdapter().loadXML( new InputSource( is ));

  /** loads XML from given Reader */
  final def load( reader:Reader ): scala.xml.Elem =
    new NoBindingFactoryAdapter().loadXML( new InputSource( reader ));

  /** loads XML from given sysID */
  final def load( sysID:String ): scala.xml.Elem =
    new NoBindingFactoryAdapter().loadXML( new InputSource( sysID ));

  /** loads XML from a given input source */
  final def load( source:InputSource ): scala.xml.Elem =
    new NoBindingFactoryAdapter().loadXML( source );

  /** loads XML from a string */
  final def loadString( string:String ): scala.xml.Elem =
    load(new StringReader(string))

  /** saves XML to filename with encoding ISO-8859-1. Does not write an xml-decl or doctype. */
  final def save(filename: String, doc: Elem): Unit =
    save(filename, doc, "ISO-8859-1");

  /** saves XML to filename with given encoding. Does not write an xml-decl or doctype. */
  final def save(filename: String, doc: Elem, enc: String): Unit =
    saveFull(filename, doc, enc, false, null);

  final def saveFull(filename: String, doc: Document, xmlDecl: Boolean, doctype: dtd.DocType): Unit =
    saveFull(filename, doc, "ISO-8859-1", xmlDecl, doctype);

  final def saveFull(filename: String, doc: Document, enc: String, xmlDecl: Boolean, doctype: dtd.DocType): Unit = {
    /* using NIO classes of JDK 1.4 */
    import java.io.{FileOutputStream,Writer};
    import java.nio.channels.{Channels,FileChannel};

    val fos = new FileOutputStream( filename );
    val w: Writer = Channels.newWriter( fos.getChannel(), enc );

    /* 2do: optimize by giving writer parameter to toXML*/
    if(xmlDecl) w.write( "<?xml version='1.0' encoding='"+enc+"'?>\n");
    if(doctype!=null) w.write( doctype.toString()+"\n");
    w.write( Utility.toXML( doc.docElem ));

    w.close();
    fos.close();

  }

}
