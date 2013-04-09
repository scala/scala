/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml
package include.sax

import scala.collection.mutable
import org.xml.sax.{ ContentHandler, XMLReader, Locator, Attributes }
import org.xml.sax.ext.LexicalHandler
import java.io.{ File, OutputStream, OutputStreamWriter, Writer, IOException }

/** XIncluder is a SAX `ContentHandler` that writes its XML document onto
 * an output stream after resolving all `xinclude:include` elements.
 *
 * Based on Eliotte Rusty Harold's SAXXIncluder.
 */
class XIncluder(outs: OutputStream, encoding: String) extends ContentHandler with LexicalHandler {

  var out = new OutputStreamWriter(outs, encoding)

  def setDocumentLocator(locator: Locator) {}

  def startDocument() {
    try {
      out.write("<?xml version='1.0' encoding='"
                + encoding + "'?>\r\n")
    }
    catch {
      case e:IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  def endDocument() {
    try {
      out.flush()
    }
    catch {
      case e:IOException =>
        throw new SAXException("Flush failed", e)
    }
  }

  def startPrefixMapping(prefix: String , uri: String) {}

  def endPrefixMapping(prefix: String) {}

  def startElement(namespaceURI: String, localName: String, qualifiedName: String, atts: Attributes) = {
    try {
      out.write("<" + qualifiedName)
      var i = 0; while (i < atts.getLength()) {
        out.write(" ")
        out.write(atts.getQName(i))
        out.write("='")
        val value = atts.getValue(i)
        // @todo Need to use character references if the encoding
        // can't support the character
        out.write(scala.xml.Utility.escape(value))
        out.write("'")
        i += 1
      }
      out.write(">")
    }
    catch {
      case e:IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  def endElement(namespaceURI: String, localName:String, qualifiedName: String) {
    try {
      out.write("</" + qualifiedName + ">")
    }
    catch {
      case e: IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  // need to escape characters that are not in the given
  // encoding using character references????
  def characters(ch: Array[Char], start: Int, length: Int) {
    try {
      var  i = 0; while (i < length) {
        val c = ch(start+i)
        if (c == '&') out.write("&amp;")
        else if (c == '<') out.write("&lt;")
        // This next fix is normally not necessary.
        // However, it is required if text contains ]]>
        // (The end CDATA section delimiter)
        else if (c == '>') out.write("&gt;")
        else out.write(c)
        i += 1
      }
    }
    catch {
      case e: IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  def  ignorableWhitespace(ch: Array[Char], start: Int , length: Int) {
    this.characters(ch, start, length)
  }

  // do I need to escape text in PI????
  def processingInstruction(target: String, data: String) {
    try {
      out.write("<?" + target + " " + data + "?>")
    }
    catch {
      case e:IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  def skippedEntity(name: String) {
    try {
      out.write("&" + name + ";")
    }
    catch {
      case e:IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  // LexicalHandler methods
  private var inDTD: Boolean = false
  private val entities = new mutable.Stack[String]()

  def startDTD(name: String, publicID: String, systemID: String) {
    inDTD = true
    // if this is the source document, output a DOCTYPE declaration
    if (entities.isEmpty) {
      var id = ""
      if (publicID != null) id = " PUBLIC \"" + publicID + "\" \"" + systemID + '"'
      else if (systemID != null) id = " SYSTEM \"" + systemID + '"'
      try {
        out.write("<!DOCTYPE " + name + id + ">\r\n")
      }
      catch {
        case e:IOException =>
          throw new SAXException("Error while writing DOCTYPE", e)
      }
    }
  }
  def endDTD() {}

  def startEntity(name: String) {
    entities push name
  }

  def endEntity(name: String) {
    entities.pop()
  }

  def startCDATA() {}
  def endCDATA() {}

  // Just need this reference so we can ask if a comment is
  // inside an include element or not
  private var filter: XIncludeFilter = null

  def setFilter(filter: XIncludeFilter) {
    this.filter = filter
  }

  def comment(ch: Array[Char], start: Int, length: Int) {
    if (!inDTD && !filter.insideIncludeElement()) {
      try {
        out.write("<!--")
        out.write(ch, start, length)
        out.write("-->")
      }
      catch {
        case e: IOException =>
          throw new SAXException("Write failed", e)
      }
    }
  }
}
