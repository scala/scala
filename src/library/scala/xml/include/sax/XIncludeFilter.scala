/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml
package include.sax
import scala.xml.include._

import org.xml.sax.Attributes
import org.xml.sax.SAXException
import org.xml.sax.XMLReader
import org.xml.sax.EntityResolver
import org.xml.sax.Locator
import org.xml.sax.helpers.XMLReaderFactory
import org.xml.sax.helpers.XMLFilterImpl
import org.xml.sax.helpers.NamespaceSupport
import org.xml.sax.helpers.AttributesImpl

import java.net.URL
import java.net.URLConnection
import java.net.MalformedURLException
import java.io.UnsupportedEncodingException
import java.io.IOException
import java.io.InputStream
import java.io.BufferedInputStream
import java.io.InputStreamReader
import java.util.Stack

/**
 * <p>
 *  This is a SAX filter which resolves all XInclude include elements
 *  before passing them on to the client application. Currently this
 *  class has the following known deviation from the XInclude specification:
 * </p>
 *  <ol>
 *   <li>XPointer is not supported.</li>
 *  </ol>
 *
 *  <p>
 *    Furthermore, I would definitely use a new instance of this class
 *    for each document you want to process. I doubt it can be used
 *    successfully on multiple documents. Furthermore, I can virtually
 *    guarantee that this class is not thread safe. You have been
 *    warned.
 *  </p>
 *
 *  <p>
 *    Since this class is not designed to be subclassed, and since
 *    I have not yet considered how that might affect the methods
 *    herein or what other protected methods might be needed to support
 *    subclasses, I have declared this class final. I may remove this
 *    restriction later, though the use-case for subclassing is weak.
 *    This class is designed to have its functionality extended via a
 *    a horizontal chain of filters, not a
 *    vertical hierarchy of sub and superclasses.
 *  </p>
 *
 *  <p>
 *    To use this class:
 *  </p>
 *  <ol>
 *   <li>Construct an <code>XIncludeFilter</code> object with a known base URL</li>
 *   <li>Pass the <code>XMLReader</code> object from which the raw document will
 *       be read to the <code>setParent()</code> method of this object. </li>
 *   <li>Pass your own <code>ContentHandler</code> object to the
 *       <code>setContentHandler()</code> method of this object. This is the
 *       object which will receive events from the parsed and included
 *       document.
 *   </li>
 *   <li>Optional: if you wish to receive comments, set your own
 *       <code>LexicalHandler</code> object as the value of this object's
 *       http://xml.org/sax/properties/lexical-handler property.
 *       Also make sure your <code>LexicalHandler</code> asks this object
 *       for the status of each comment using <code>insideIncludeElement</code>
 *       before doing anything with the comment.
 *   </li>
 *   <li>Pass the URL of the document to read to this object's
 *       <code>parse()</code> method</li>
 *  </ol>
 *
 *  <p> e.g.</p>
 *  <pre><code>XIncludeFilter includer = new XIncludeFilter(base);
 *  includer.setParent(parser);
 *  includer.setContentHandler(new SAXXIncluder(System.out));
 *  includer.parse(args[i]);</code>
 *  </pre>
 * </p>
 * translated from Elliotte Rusty Harold's Java source
 * @author Burak Emir
 */
class XIncludeFilter extends XMLFilterImpl {

  final val  XINCLUDE_NAMESPACE = "http://www.w3.org/2001/XInclude";

  private val bases = new Stack[URL]();
  private val locators = new Stack[Locator]();

/*    private EntityResolver resolver;

    public XIncludeFilter() {
        this(null);
    }

    public XIncludeFilter(EntityResolver resolver) {
        this.resolver = resolver;
    }   */


    // what if this isn't called????
    // do I need to check this in startDocument() and push something
    // there????
  override def setDocumentLocator(locator: Locator) {
    locators.push(locator)
    val base = locator.getSystemId()
    try {
      bases.push(new URL(base))
    }
    catch {
      case e:MalformedURLException =>
        throw new UnsupportedOperationException("Unrecognized SYSTEM ID: " + base)
    }
    super.setDocumentLocator(locator)
  }


  // necessary to throw away contents of non-empty XInclude elements
  private var level = 0

  /**
    * <p>
    * This utility method returns true if and only if this reader is
    * currently inside a non-empty include element. (This is <strong>
    * not</strong> the same as being inside the node set which replaces
    * the include element.) This is primarily needed for comments
    * inside include elements. It must be checked by the actual
    * LexicalHandler to see whether a comment is passed or not.
    * </p>
    *
    * @return boolean
    */
  def insideIncludeElement(): Boolean = level != 0

  override def startElement(uri: String, localName: String, qName: String, atts1: Attributes) {
    var atts = atts1
    if (level == 0) { // We're not inside an xi:include element

      // Adjust bases stack by pushing either the new
      // value of xml:base or the base of the parent
      val base = atts.getValue(NamespaceSupport.XMLNS, "base")
      val parentBase = bases.peek().asInstanceOf[URL]
      var currentBase = parentBase
      if (base != null) {
        try {
          currentBase = new URL(parentBase, base)
        }
        catch {
          case e: MalformedURLException =>
            throw new SAXException("Malformed base URL: "
                                   + currentBase, e)
        }
      }
      bases.push(currentBase);

      if (uri.equals(XINCLUDE_NAMESPACE) && localName.equals("include")) {
        // include external document
        val href = atts.getValue("href")
        // Verify that there is an href attribute
        if (href==null) {
          throw new SAXException("Missing href attribute")
        }

        var parse = atts.getValue("parse")
        if (parse == null) parse = "xml"

        if (parse.equals("text")) {
          val encoding = atts.getValue("encoding");
          includeTextDocument(href, encoding);
        }
        else if (parse.equals("xml")) {
          includeXMLDocument(href);
        }
        // Need to check this also in DOM and JDOM????
        else {
          throw new SAXException(
            "Illegal value for parse attribute: " + parse);
        }
        level += 1
      }
      else {
        if (atRoot) {
          // add xml:base attribute if necessary
          val attsImpl = new AttributesImpl(atts)
          attsImpl.addAttribute(NamespaceSupport.XMLNS, "base",
                                "xml:base", "CDATA", currentBase.toExternalForm())
          atts = attsImpl
          atRoot = false
        }
        super.startElement(uri, localName, qName, atts)
      }
    }
  }

  override def endElement(uri: String, localName: String, qName: String) {
    if (uri.equals(XINCLUDE_NAMESPACE)
        && localName.equals("include")) {
          level -= 1;
    }
    else if (level == 0) {
      bases.pop()
      super.endElement(uri, localName, qName)
    }
  }

  private var depth = 0;

  override def startDocument() {
    level = 0
    if (depth == 0) super.startDocument()
    depth += 1
  }

  override def endDocument() {
    locators.pop()
    bases.pop(); // pop the URL for the document itself
    depth -= 1
    if (depth == 0) super.endDocument()
  }

  // how do prefix mappings move across documents????
  override def startPrefixMapping(prefix: String , uri: String) {
    if (level == 0) super.startPrefixMapping(prefix, uri)
  }

  override def endPrefixMapping(prefix: String) {
    if (level == 0) super.endPrefixMapping(prefix)
  }

  override def characters(ch: Array[Char], start: Int, length: Int) {
    if (level == 0) super.characters(ch, start, length)
  }

  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int) {
    if (level == 0) super.ignorableWhitespace(ch, start, length)
  }

  override def processingInstruction(target: String, data: String) {
    if (level == 0) super.processingInstruction(target, data)
  }

  override def skippedEntity(name: String) {
    if (level == 0) super.skippedEntity(name)
  }

  // convenience method for error messages
  private def getLocation(): String = {
    var locationString = ""
    val locator = locators.peek().asInstanceOf[Locator]
    var publicID = ""
    var systemID = ""
    var column = -1
    var line = -1
    if (locator != null) {
      publicID = locator.getPublicId()
      systemID = locator.getSystemId()
      line = locator.getLineNumber()
      column = locator.getColumnNumber()
    }
    locationString = (" in document included from " + publicID
    + " at " + systemID
    + " at line " + line + ", column " + column);

    locationString
  }

  /**
    * <p>
    * This utility method reads a document at a specified URL
    * and fires off calls to <code>characters()</code>.
    * It's used to include files with <code>parse="text"</code>
    * </p>
    *
    * @param  url          URL of the document that will be read
    * @param  encoding     Encoding of the document; e.g. UTF-8,
    *                      ISO-8859-1, etc.
    * @return void
    * @throws SAXException if the requested document cannot
                           be downloaded from the specified URL
                           or if the encoding is not recognized
    */
  private def includeTextDocument(url: String, encoding1: String) {
    var encoding = encoding1
    if (encoding == null || encoding.trim().equals("")) encoding = "UTF-8";
    var source: URL = null
    try {
      val base = bases.peek().asInstanceOf[URL]
      source = new URL(base, url)
    }
    catch {
      case e: MalformedURLException =>
        val ex = new UnavailableResourceException("Unresolvable URL " + url
                                                  + getLocation());
      ex.setRootCause(e);
      throw new SAXException("Unresolvable URL " + url + getLocation(), ex);
    }

    try {
      val uc = source.openConnection()
      val in = new BufferedInputStream(uc.getInputStream())
      var encodingFromHeader = uc.getContentEncoding()
      var contentType = uc.getContentType()
      if (encodingFromHeader != null)
        encoding = encodingFromHeader
      else {
        // What if file does not have a MIME type but name ends in .xml????
        // MIME types are case-insensitive
        // Java may be picking this up from file URL
        if (contentType != null) {
          contentType = contentType.toLowerCase();
          if (contentType.equals("text/xml")
              || contentType.equals("application/xml")
              || (contentType.startsWith("text/") && contentType.endsWith("+xml") )
              || (contentType.startsWith("application/") && contentType.endsWith("+xml"))) {
                encoding = EncodingHeuristics.readEncodingFromStream(in);
              }
        }
      }
      val reader = new InputStreamReader(in, encoding)
      val c = new Array[Char](1024)
      var charsRead: Int = 0  // bogus init value
      do {
        charsRead = reader.read(c, 0, 1024);
        if (charsRead > 0) this.characters(c, 0, charsRead);
      } while (charsRead != -1) ;
    }
    catch {
      case e: UnsupportedEncodingException =>
        throw new SAXException("Unsupported encoding: "
                               + encoding + getLocation(), e);
      case e: IOException =>
        throw new SAXException("Document not found: "
                               + source.toExternalForm() + getLocation(), e);
    }

  }

  private var atRoot = false

  /**
    * <p>
    * This utility method reads a document at a specified URL
    * and fires off calls to various <code>ContentHandler</code> methods.
    * It's used to include files with <code>parse="xml"</code>
    * </p>
    *
    * @param  url          URL of the document that will be read
    * @return void
    * @throws SAXException if the requested document cannot
                           be downloaded from the specified URL.
    */
  private def includeXMLDocument(url: String) {
    var source: URL = null
    try {
      val base = bases.peek().asInstanceOf[URL]
      source = new URL(base, url)
    }
    catch {
      case e:MalformedURLException =>
        val ex = new UnavailableResourceException("Unresolvable URL " + url
                                                  + getLocation());
        ex.setRootCause(e)
        throw new SAXException("Unresolvable URL " + url + getLocation(), ex)
    }

    try {
      // make this more robust
      var parser: XMLReader = null
      try {
        parser = XMLReaderFactory.createXMLReader()
      } catch {
        case e:SAXException =>
          try {
            parser = XMLReaderFactory.createXMLReader(
              "org.apache.xerces.parsers.SAXParser"
            );
          } catch {
            case e2: SAXException =>
              System.err.println("Could not find an XML parser")
          }
      }
      if(parser != null) {
        parser.setContentHandler(this)
        val resolver = this.getEntityResolver()
        if (resolver != null) parser.setEntityResolver(resolver);
        // save old level and base
        val previousLevel = level
        this.level = 0
        if (bases.contains(source)) {
          val e = new CircularIncludeException(
            "Circular XInclude Reference to " + source + getLocation()
          );
          throw new SAXException("Circular XInclude Reference", e)
        }
        bases.push(source)
        atRoot = true
        parser.parse(source.toExternalForm())
        // restore old level and base
        this.level = previousLevel
        bases.pop()
      }
    }
    catch {
      case e:IOException =>
        throw new SAXException("Document not found: "
                               + source.toExternalForm() + getLocation(), e)
    }

  }
}
