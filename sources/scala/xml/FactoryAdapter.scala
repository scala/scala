package scala.xml ;


import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

import java.util.Map;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Stack;
import java.util.Iterator;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;


import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;

import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.ParserAdapter;

import org.xml.sax.helpers.XMLReaderFactory;

abstract class FactoryAdapter
	 extends DefaultHandler()
	 // with ContentHandler
	 // with ErrorHandler  // SAX2
{
  // default settings

  /** Default parser name. */
  val DEFAULT_PARSER_NAME   = "org.apache.crimson.parser.XMLReaderImpl"; // included in JDK1.4
  //val DEFAULT_PARSER_NAME   = "org.apache.xerces.parsers.SAXParser";

  /** Namespaces feature id (http://xml.org/sax/features/namespaces). */
  val NAMESPACES_FEATURE_ID = "http://xml.org/sax/features/namespaces";

  //
  // Constructors
  //

  val buffer = new StringBuffer();
  val attribStack = new Stack();
  val hStack  = new Stack();   // [ element ] contains siblings
  val tagStack  = new Stack(); // [String]

  var curTag : String = null ;
  var capture:boolean = false;

  // abstract methods

  /** Tests if an XML element contains text.
  * @return true if element named <code>localName</code> contains text.
  */
   def elementContainsText( localName:String ):boolean ; // abstract

  /** Creates an new XML element.
  * @param elemName
  * @param attribs
  * @param chIter
  * @return a new XML element.
  */
  def createElement(elemName:String ,
                    attribs:Map ,
                    chIter:Iterator ):scala.Object; //abstract

  /** Creates an PCDATA element.
   * @param text
   * @return a new PCDATA element.
   */
  def createPCDATA( text:String ):scala.Object; // abstract

  //
  // ContentHandler methods
  //

  /** Set document locator.
   * @param locator
   */
  //def setDocumentLocator( locator:Locator ):Unit = {}

  /** Start document.
  * @throws org.xml.sax.SAXException if ..
  */
  //def startDocument():Unit /*throws SAXException*/ = {}

  /** Characters.
  * @param ch
  * @param offset
  * @param length
  */
   override def characters(ch:Array[char] , offset:int , length:int ):Unit
  /*throws SAXException*/ = {

        var ws:boolean = false;

        //System.err.println( "before \""+buffer+"\"" );

        if (capture) {
	  var i:int = offset;
	  while (i < offset + length) {
            if ( Character.isWhitespace( ch(i) ) ) {
              if (!ws) {
                buffer.append(' ');
                ws = true;
              }
            } else {
              buffer.append(ch(i));
              ws = false;
            }
	    i = i+1;
          }
	}
        //System.err.println( "after \""+buffer+"\"" );
        //System.err.println();

    } // characters(char[],int,int)

    /** Ignorable whitespace.
     */
    //def ignorableWhitespace(ch:Array[char] , offset:int , length:int ):Unit
    //    /*throws SAXException*/ = {}

    /** End document.
     */
    //def endDocument():Unit /*throws SAXException*/ = {}

    //
    // ContentHandler methods
    //

    /** Start prefix mapping.
     */
    //def startPrefixMapping( prefix:String , uri:String ):Unit
    //    /*throws SAXException*/ = {}

    /** Start element.
     */
    override def startElement( uri:String , localName:String , qname:String ,
                             attributes:Attributes ):Unit /*throws SAXException*/ = {

        captureText();

        tagStack.push(curTag);
        curTag = localName ;

        capture = elementContainsText(localName) ;

        hStack.push( null );
        var map:HashMap = null;

        if (attributes == null) {
              //fOut.println("null");
        }
        else {
              map = new HashMap();

              val length:int = attributes.getLength();
	      var i : int = 0;
              while ( i < length ) {
                    val attrLocalName = attributes.getLocalName(i);
                    //String attrQName = attributes.getQName(i);
                    //String attrURI = attributes.getURI(i);
                    val attrType = attributes.getType(i);
                    val attrValue = attributes.getValue(i);
                    if( attrType.equals("CDATA") )
                          { val _ = map.put( attrLocalName, attrValue ); }
		i=i+1;
              }
	}

        val _ = attribStack.push( map );

    } // startElement(String,String,String,Attributes)


    /** this way to deal with whitespace is quite nonstandard, but useful
     *
     */
    def captureText():Unit = {
        if (capture == true) {
	    val text = buffer.toString();
	    if(( text.length() > 0 )&&( !( text.equals(" ")))) {
		val _ = hStack.push( createPCDATA( text ) );
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
    override def endElement(uri:String , localName:String , qname:String ):Unit
            /*throws SAXException*/ = {

        captureText();

        val attribMap = attribStack.pop().asInstanceOf[ HashMap ];

        // reverse order to get it right
        val v       = new LinkedList();
        var child = hStack.pop().asInstanceOf[ scala.Object ];
        while( child != null ) {
            v.addFirst(child);
            child = hStack.pop().asInstanceOf[ scala.Object ];
        }

        // create element
        rootElem = createElement(localName, attribMap, v.iterator());
        hStack.push(rootElem);

        // set
        curTag = tagStack.pop().asInstanceOf[ String ];

        if (curTag != null) // root level
            capture = elementContainsText(curTag);
        else
            capture = false;

    } // endElement(String,String,String)

    /** End prefix mapping.
     */
    //def endPrefixMapping(prefix:String ):Unit /*throws SAXException*/ = {}

    /** Skipped entity.
     */
    //def skippedEntity(name:String ):Unit /*throws SAXException*/ = {}

    //
    // ErrorHandler methods
    //

    /** Warning.*/
    override def warning(ex:SAXParseException ):Unit
      /*throws SAXException*/ = {
	// ignore warning, crimson warns even for entity resolution!
        //printError("Warning", ex);
    }
    /** Error.     */
    override def error(ex:SAXParseException ):Unit
    /*throws SAXException*/ = {
        printError("Error", ex);
    }

    /** Fatal error.
     */
    override def  fatalError(ex:SAXParseException ):Unit
  /*throws SAXException*/ = {
        printError("Fatal Error", ex);
        /*throw ex;*/
    }

    //
    // Protected methods
    //

    /** Prints the error message.
     */
    protected def printError( errtype:String , ex:SAXParseException ):Unit = {

        System.err.print("[");
        System.err.print(errtype);
        System.err.print("] ");

        var systemId = ex.getSystemId();
        if (systemId != null) {
            val index = systemId.lastIndexOf('/');
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

    } // printError(String,SAXParseException)
  //} // class FA_ErrorHandler
    var rootElem : scala.Object = null;

    //FactoryAdapter
    // MAIN
    //

    /** load XML document
     * @param fileName
     * @return a new XML document object
     */
    def  loadXML( fileName:String ):scala.Object = {

        // variables
        //PrintWriter out = new PrintWriter(System.out);
        var parser:XMLReader  = null;

        // use default parser
        // create parser
        try {
             parser = XMLReaderFactory.createXMLReader(DEFAULT_PARSER_NAME);
        } catch {
	  case ( e:Exception ) => {
            System.err.println("error: Unable to instantiate parser (" +
                               DEFAULT_PARSER_NAME + ")");
            System.exit(-1);
          }
	}

        // set parser features
        try {
            parser.setFeature(NAMESPACES_FEATURE_ID, true);
        } catch {
	  case ( e:SAXException ) => {
            System.err.println("warning: Parser does not support feature (" +
                               NAMESPACES_FEATURE_ID + ")");
          }
	}
        // set handlers
        parser.setErrorHandler( this /*FA_ErrorHandler*/ );

        if (parser.isInstanceOf[ XMLReader ]) {
            parser.setContentHandler(this);
        }
        else {
            System.err.println("wrong parser");
            System.exit(-1);
        }

        // parse file
        try {
            //System.err.println("[parsing \"" + fileName + "\"]");
            parser.parse( fileName );
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
      rootElem;

    } // loadXML


}
