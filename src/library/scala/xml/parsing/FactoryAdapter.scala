/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.parsing;

import java.io._ ;
import scala.collection.mutable.{HashMap,Stack};

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;

import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import org.xml.sax.InputSource;

import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import javax.xml.parsers.SAXParserFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;


/** SAX adapter class, for use with Java SAX parser. Keeps track of
 *  namespace bindings, without relying on namespace handling of the
 *  underlying SAX parser.
 */
abstract class FactoryAdapter extends DefaultHandler() {

  val buffer      = new StringBuffer();
  val attribStack = new Stack[MetaData];
  val hStack      = new Stack[Node];   // [ element ] contains siblings
  val tagStack    = new Stack[String];
  var scopeStack  = new Stack[NamespaceBinding];

  var curTag : String = null ;
  var capture: Boolean = false;

  // abstract methods

  /** Tests if an XML element contains text.
   * @return true if element named <code>localName</code> contains text.
   */
  def nodeContainsText(localName: String): Boolean ; // abstract

  /** creates an new non-text(tree) node.
   * @param elemName
   * @param attribs
   * @param chIter
   * @return a new XML element.
   */
  def createNode(pre: String, elemName: String, attribs: MetaData, scope: NamespaceBinding, chIter: List[Node] ):Node; //abstract

  /** creates a Text node.
   * @param text
   * @return a new Text node.
   */
  def createText( text:String ):Text; // abstract

  //
  // ContentHandler methods
  //

  val normalizeWhitespace = false;

  /** Characters.
  * @param ch
  * @param offset
  * @param length
  */
   override def characters(ch: Array[Char], offset: Int, length: Int): Unit = {

        if (capture) {
          if( normalizeWhitespace ) { // normalizing whitespace is not compliant, but useful */
	    var i: Int = offset
            var ws = false
	    while (i < offset + length) {
              if ( Character.isWhitespace( ch(i) ) ) {
                if (!ws) {
                  buffer.append(' ')
                  ws = true
                }
              } else {
                buffer.append(ch(i))
                ws = false
              }
	      i = i+1
            }
          } else { // compliant:report every character

              buffer.append( ch, offset, length )

          }
	}
   }

    //var elemCount = 0; //STATISTICS

    /* ContentHandler methods */

    /* Start prefix mapping - use default impl.
     def startPrefixMapping( prefix:String , uri:String ):Unit = {}
     */



    /* Start element. */
    override def startElement(uri:String, _localName:String, qname:String, attributes:Attributes ):Unit = {
      /*elemCount = elemCount + 1; STATISTICS */
      captureText();
      //Console.println("FactoryAdapter::startElement("+uri+","+_localName+","+qname+","+attributes+")");
      tagStack.push(curTag);
      curTag = qname; //localName ;

      val colon = qname.indexOf(':'.asInstanceOf[Int]);
      val localName = if(-1 == colon) qname else qname.substring(colon+1,qname.length());

      //Console.println("FactoryAdapter::startElement - localName ="+localName);

      capture = nodeContainsText(localName) ;

      hStack.push( null );
      var m: MetaData = Null;

      var scpe = scopeStack.top;
      for( val i <- List.range( 0, attributes.getLength() )) {
        //val attrType = attributes.getType(i); // unused for now
        val qname = attributes.getQName(i);
        val value = attributes.getValue(i);
        val colon = qname.indexOf(':'.asInstanceOf[Int]);
        if(-1 != colon) {                     // prefixed attribute
          val pre = qname.substring(0, colon);
          val key = qname.substring(colon+1, qname.length());
          if("xmlns" /*XML.xmlns*/ == pre)
            scpe = value.length() match {
              case 0 => new NamespaceBinding(key, null,  scpe);
              case _ => new NamespaceBinding(key, value, scpe);
            }
          else
              m = new PrefixedAttribute(pre, key, value, m);
        } else if("xmlns" /*XML.xmlns*/ == qname)
          scpe = value.length() match {
            case 0 => new NamespaceBinding(null, null,  scpe);
            case _ => new NamespaceBinding(null, value, scpe);
          }
          else
            m = new UnprefixedAttribute(qname, value, m)
      }
      scopeStack.push(scpe);
      attribStack.push( m );
      {}
    } // startElement(String,String,String,Attributes)


    /** captures text, possibly normalizing whitespace
     */
    def captureText():Unit = {
        if (capture == true) {
	    val text = buffer.toString();
	    if(( text.length() > 0 )&&( !( text.equals(" ")))) {
		val _ = hStack.push( createText( text ) );
	    }
        }
        buffer.setLength(0);
    }

    /** End element.
     * @param uri
     * @param localName
     * @param qname
     * @throws org.xml.sax.SAXException if ..
     */
    override def endElement(uri:String , _localName:String , qname:String ):Unit = {
      captureText();

      val metaData = attribStack.pop;

      // reverse order to get it right
      var v:List[Node] = Nil;
      var child:Node = hStack.pop;
      while( child != null ) {
        v = child::v;
        child = hStack.pop;
      }

      val colon = qname.indexOf(':'.asInstanceOf[Int]);
      val localName = if(-1 == colon) qname else qname.substring(colon+1,qname.length());

      val scp = scopeStack.pop;
      // create element
      rootElem = if(-1 == colon)
          createNode( null, localName, metaData, scp, v );
        else
          createNode( qname.substring(0,colon), localName, metaData, scp, v );

      hStack.push(rootElem);

      // set
      curTag = tagStack.pop;

      if (curTag != null) // root level
        capture = nodeContainsText(curTag);
      else
        capture = false;

    } // endElement(String,String,String)

    //
    // ErrorHandler methods
    //

    /** Warning.*/
    override def warning(ex:SAXParseException ):Unit  = {
	// ignore warning, crimson warns even for entity resolution!
        //printError("Warning", ex);
    }
    /** Error.     */
    override def error(ex:SAXParseException ):Unit = {
        printError("Error", ex);
    }

    /** Fatal error.*/
    override def  fatalError(ex:SAXParseException ):Unit = {
        printError("Fatal Error", ex);
    }

    //
    // Protected methods
    //

    /** Prints the error message */
    protected def printError( errtype:String , ex:SAXParseException ):Unit = {

        System.err.print("[");
        System.err.print(errtype);
        System.err.print("] ");

        var systemId = ex.getSystemId();
        if (systemId != null) {
            val index = systemId.lastIndexOf('/'.asInstanceOf[Int]);
            if (index != -1)
                systemId = systemId.substring(index + 1);
            //System.err.print(systemId);
        }

        System.err.print(':');
        System.err.print(ex.getLineNumber());
        System.err.print(':');
        System.err.print(ex.getColumnNumber());
        System.err.print(": ");
        System.err.print(ex.getMessage());
        System.err.println();
        System.err.flush();

    }

    var rootElem : Node = null:Node;

    //FactoryAdapter
    // MAIN
    //

    /** load XML document
     * @param source
     * @return a new XML document object
     */
    def  loadXML( source:InputSource ):Node = {

      // variables
      var parser:SAXParser  = null;

      // create parser
      try {
        val f = SAXParserFactory.newInstance();
        f.setNamespaceAware( false );
        parser = f.newSAXParser();
      } catch {
	case ( e:Exception ) => {
          System.err.println("error: Unable to instantiate parser");
          System.exit(-1);
        }
      }

      // parse file
      try {
        //System.err.println("[parsing \"" + source + "\"]");
        scopeStack.push(TopScope);
        parser.parse( source, this );
        scopeStack.pop;
      } catch {
        case ( e:SAXParseException ) => {
          // ignore
        }
        case ( e:Exception ) => {
          System.err.println("error: Parse error occurred - " + e.getMessage());
          if (e.isInstanceOf[ SAXException ]) {
            (e.asInstanceOf[ SAXException ])
	    .getException()
	    .printStackTrace( System.err );
          } else {
            e.printStackTrace(System.err);
	  }
        }
      } // catch
      //System.err.println("[FactoryAdapter: total #elements = "+elemCount+"]");
      rootElem

    } // loadXML



  /** loads XML from given file */
  def loadFile( file:File ):Node = loadXML( new InputSource(
    new FileInputStream( file )
  ));

  /** loads XML from given file descriptor */
  def loadFile( fileDesc:FileDescriptor ):Node = loadXML( new InputSource(
    new FileInputStream( fileDesc )
  ));

  /** loads XML from given file */
  def loadFile( fileName:String ):Node = loadXML( new InputSource(
    new FileInputStream( fileName )
  ));

  /** loads XML from given InputStream */
  def load( is:InputStream ):Node = loadXML( new InputSource( is ));

  /** loads XML from given Reader */
  def load( reader:Reader ):Node = loadXML( new InputSource( reader ));

  /** loads XML from given sysID */
  def load( sysID:String ):Node = loadXML( new InputSource( sysID ));

}
