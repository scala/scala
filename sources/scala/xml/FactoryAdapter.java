package scala.xml;
// MIT License

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


/** 2do:
 * - handle MixedContent properly !
 *  - ? canonical XML ?
 *  - performance ( java Array <-> scala.Array )
 */
public abstract class FactoryAdapter
    extends DefaultHandler
    implements ContentHandler, ErrorHandler // SAX2
{

    // default settings

    /** Default parser name. */
    protected static final String
        DEFAULT_PARSER_NAME = "org.apache.xerces.parsers.SAXParser";

    /** Namespaces feature id (http://xml.org/sax/features/namespaces). */
    protected static final String
        NAMESPACES_FEATURE_ID = "http://xml.org/sax/features/namespaces";

    //
    // Constructors
    //

    /** Default constructor. */
    public FactoryAdapter() {
        buffer = new StringBuffer();
        attribStack = new Stack();
        hStack = new Stack();
        tagStack = new Stack();
        //fOut = new PrintWriter(System.out);
    } // <init>()


    // Data

    Stack hStack;   // [ element ] contains siblings
    Stack tagStack; // [String]

    Stack attribStack;

    String curTag;

    StringBuffer buffer;
    boolean capture;

    // these are defined in a scala class

    /** Tests if an XML element contains text.
     * @return true if element named <code>localName</code> contains text.
     */
    abstract boolean elementContainsText(String localName);

    /** Creates an new XML element.
     * @param elemName
     * @param attribs
     * @param chIter
     * @return a new XML element.
     */
    abstract scala.Object createElement(String elemName,
                                        Map attribs,
                                        Iterator chIter);

    /** Creates an PCDATA element.
     * @param text
     * @return a new PCDATA element.
     */
    abstract scala.Object createPCDATA(String text);

    //
    // ContentHandler methods
    //

    /** Set document locator.
     * @param locator
     */
    public void setDocumentLocator(Locator locator) {
    }

    /** Start document.
     * @throws org.xml.sax.SAXException if ..
     */
    public void startDocument() throws SAXException {
    }


    /** Characters.
     * @param ch
     * @param offset
     * @param length
     */
    public void characters(char[] ch, int offset, int length)
            throws SAXException {

        boolean ws = false;

        //System.err.println( "before \""+buffer+"\"" );

        if (capture)
            for (int i = offset; i < offset + length; i++)
                if (Character.isWhitespace(ch[i])) {
                    if (!ws) {
                        buffer.append(' ');
                        ws = true;
                    }
                } else {
                    buffer.append(ch[i]);
                    ws = false;
                }

        //System.err.println( "after \""+buffer+"\"" );
        //System.err.println();

    } // characters(char[],int,int)

    /** Ignorable whitespace.
     */
    public void ignorableWhitespace(char[] ch, int offset, int length)
        throws SAXException {
    }

    /** End document.
     */
    public void endDocument() throws SAXException {
    }

    //
    // ContentHandler methods
    //

    /** Start prefix mapping.
     */
    public void startPrefixMapping(String prefix, String uri)
        throws SAXException {
    }

    /** Start element.
     */
    public void startElement(String uri, String localName, String qname,
                             Attributes attributes) throws SAXException {

        captureText();

        tagStack.push(curTag);
        curTag = localName ;

        capture = elementContainsText(localName) ;

        hStack.push( null );
        HashMap map = null;

        if (attributes == null) {
              //fOut.println("null");
        }
        else {
              map = new HashMap();

              int length = attributes.getLength();
              for (int i = 0; i < length; i++) {
                    String attrLocalName = attributes.getLocalName(i);
                    //String attrQName = attributes.getQName(i);
                    //String attrURI = attributes.getURI(i);
                    String attrType = attributes.getType(i);
                    String attrValue = attributes.getValue(i);
                    if( attrType.equals("CDATA") )
                          map.put( attrLocalName, attrValue );

              }
        }
        attribStack.push( map );

    } // startElement(String,String,String,Attributes)


    /** this way to deal with whitespace is quite nonstandard, but useful
     */
    void captureText() {
        if (capture == true) {
             hStack.push(createPCDATA(buffer.toString()));
        }
        buffer.setLength(0);
    }

    /** End element.
     * @param uri
     * @param localName
     * @param qname
     * @throws org.xml.sax.SAXException if ..
     */
    public void endElement(String uri, String localName, String qname)
            throws SAXException {

        captureText();

        Map attribMap = (HashMap) attribStack.pop();

        // reverse order to get it right
        LinkedList v       = new LinkedList();
        scala.Object child = (scala.Object) hStack.pop();
        while( child != null ) {
            v.addFirst(child);
            child = (scala.Object) hStack.pop();
        }

        // create element
        rootElem = createElement(localName, attribMap, v.iterator());
        hStack.push(rootElem);

        // set
        curTag = (String) tagStack.pop();

        if (curTag != null) // root level
            capture = elementContainsText(curTag);
        else
            capture = false;

    } // endElement(String,String,String)

    /** End prefix mapping.
     */
    public void endPrefixMapping(String prefix) throws SAXException {
    }

    /** Skipped entity.
     */
    public void skippedEntity(String name) throws SAXException {
    }

    //
    // ErrorHandler methods
    //

    /** Warning.
     */
    public void warning(SAXParseException ex) throws SAXException {
        printError("Warning", ex);
    }

    /** Error.
     */
    public void error(SAXParseException ex) throws SAXException {
        printError("Error", ex);
    }

    /** Fatal error.
     */
    public void fatalError(SAXParseException ex) throws SAXException {
        printError("Fatal Error", ex);
        throw ex;
    }

    //
    // Protected methods
    //

    /** Prints the error message.
     */
    protected void printError(String type, SAXParseException ex) {

        System.err.print("[");
        System.err.print(type);
        System.err.print("] ");

        String systemId = ex.getSystemId();
        if (systemId != null) {
            int index = systemId.lastIndexOf('/');
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

    scala.Object rootElem;

    //FactoryAdapter
    // MAIN
    //

    /** load XML document
     * @param fileName
     * @return a new XML document object
     */
    public scala.Object loadXML(String fileName) {

        // variables
        //PrintWriter out = new PrintWriter(System.out);
        XMLReader parser = null;

        // use default parser
        // create parser
        try {
             parser = XMLReaderFactory.createXMLReader(DEFAULT_PARSER_NAME);
        }
        catch (Exception e) {
            System.err.println("error: Unable to instantiate parser (" +
                               DEFAULT_PARSER_NAME + ")");
            System.exit(-1);
        }

        // set parser features
        try {
            parser.setFeature(NAMESPACES_FEATURE_ID, true);
        }
        catch (SAXException e) {
            System.err.println("warning: Parser does not support feature (" +
                               NAMESPACES_FEATURE_ID + ")");
        }

        // set handlers
        parser.setErrorHandler(this);

        if (parser instanceof XMLReader) {
            parser.setContentHandler(this);
        }
        else {
            System.err.println("wrong parser");
            System.exit(-1);
        }

        // parse file
        try {
            System.err.println("[parsing \"" + fileName + "\"]");
            parser.parse( fileName );
        }
        catch (SAXParseException e) {
            // ignore
        }
        catch (Exception e) {
            System.err.println("error: Parse error occurred - " + e.getMessage());
            if (e instanceof SAXException) {
                e = ((SAXException)e).getException();
            }
            e.printStackTrace(System.err);
        }

        return rootElem;

    } // loadXML

} // class FactoryAdapter
