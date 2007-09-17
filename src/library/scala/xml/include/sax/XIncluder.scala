package scala.xml.include.sax

import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.ContentHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.XMLReader;
import org.xml.sax.Locator;
import org.xml.sax.Attributes;
import org.xml.sax.ext.LexicalHandler;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.io.OutputStream;
import java.io.Writer;
import java.io.OutputStreamWriter;
import java.io.File;
import java.net.URL;
import java.net.MalformedURLException;
import java.util.Stack;

/** XIncluder is a SAX <code>ContentHandler</code>
 * that writes its XML document onto an output stream after resolving
 * all <code>xinclude:include</code> elements.
 *
 * <p>
 *   based on Eliotte Rusty Harold's SAXXIncluder
 * </p>
 */
class XIncluder(outs:OutputStream, encoding:String) extends Object
with ContentHandler with LexicalHandler {

  var out = new OutputStreamWriter(outs, encoding);

  def setDocumentLocator(locator: Locator): Unit = {}

  def startDocument(): Unit = {
    try {
      out.write("<?xml version='1.0' encoding='"
                + encoding + "'?>\r\n");
    }
    catch {
      case e:IOException =>
        throw new SAXException("Write failed", e);
    }
  }

  def endDocument(): Unit = {
    try {
      out.flush();
    }
    catch {
      case e:IOException =>
        throw new SAXException("Flush failed", e);
    }
  }

  def startPrefixMapping(prefix: String , uri: String): Unit = {}

  def endPrefixMapping(prefix: String): Unit = {}

  def startElement(namespaceURI: String , localName: String, qualifiedName: String , atts:Attributes ) = {
    try {
      out.write("<" + qualifiedName);
      var i = 0; while (i < atts.getLength()) {
        out.write(" ");
        out.write(atts.getQName(i));
        out.write("='");
        val value = atts.getValue(i);
        // @todo Need to use character references if the encoding
        // can't support the character
        out.write(xml.Utility.escape(value))
        out.write("'");
        i = i + 1;
      }
      out.write(">");
    }
    catch {
      case e:IOException =>
        throw new SAXException("Write failed", e);
    }
  }

  def endElement(namespaceURI: String , localName:String , qualifiedName: String ): Unit = {
    try {
      out.write("</" + qualifiedName + ">");
    }
    catch {
      case e: IOException =>
        throw new SAXException("Write failed", e);
    }

  }

  // need to escape characters that are not in the given
  // encoding using character references????
  def characters(ch: Array[char] , start: int , length: int ): Unit = {

    try {
      var  i = 0; while (i < length) {
        val c = ch(start+i);
        if (c == '&') out.write("&amp;");
        else if (c == '<') out.write("&lt;");
        // This next fix is normally not necessary.
        // However, it is required if text contains ]]>
        // (The end CDATA section delimiter)
        else if (c == '>') out.write("&gt;");
        else out.write(c);
        i = i+1;
      }
    }
    catch {
      case e: IOException =>
      throw new SAXException("Write failed", e);
    }
  }

  def  ignorableWhitespace(ch: Array[char], start: int , length: int): Unit = {
    this.characters(ch, start, length);
  }

  // do I need to escape text in PI????
  def processingInstruction(target: String , data: String): Unit = {

    try {
      out.write("<?" + target + " " + data + "?>");
    }
    catch {
      case e:IOException =>
        throw new SAXException("Write failed", e);
    }
  }

  def skippedEntity(name: String): Unit = {
    try {
      out.write("&" + name + ";");
    }
    catch {
      case e:IOException =>
        throw new SAXException("Write failed", e);
    }
  }

  // LexicalHandler methods
  private var inDTD: boolean = false;
  private val entities: Stack  = new Stack();

  def  startDTD(name: String, publicID: String, systemID: String): Unit = {
    inDTD = true;
    // if this is the source document, output a DOCTYPE declaration
    if (entities.size() == 0) {
      var id = "";
      if (publicID != null) id = " PUBLIC \"" + publicID + "\" \"" + systemID + '"';
      else if (systemID != null) id = " SYSTEM \"" + systemID + '"';
      try {
        out.write("<!DOCTYPE " + name + id + ">\r\n");
      }
      catch {
        case e:IOException =>
          throw new SAXException("Error while writing DOCTYPE", e);
      }
    }
  }
  def endDTD(): Unit =  { }

  def startEntity(name: String): Unit = {
    entities.push(name);
  }


  def endEntity(name: String): Unit = {
    entities.pop();
  }

  def startCDATA(): Unit =  {}
  def endCDATA(): Unit =  {}

  // Just need this reference so we can ask if a comment is
  // inside an include element or not
  private var filter: XIncludeFilter = null;

  def setFilter(filter: XIncludeFilter): Unit = {
    this.filter = filter;
  }

  def comment(ch: Array[char], start: int, length: int): Unit = {

    if (!inDTD && !filter.insideIncludeElement()) {
      try {
        out.write("<!--");
        out.write(ch, start, length);
        out.write("-->");
      }
      catch {
        case e:IOException =>
          throw new SAXException("Write failed", e);
      }
    }
  }
}
