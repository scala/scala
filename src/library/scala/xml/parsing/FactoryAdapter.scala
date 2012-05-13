/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml
package parsing

import java.io.{ InputStream, Reader, File, FileDescriptor, FileInputStream }
import scala.collection.{ mutable, Iterator }
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

// can be mixed into FactoryAdapter if desired
trait ConsoleErrorHandler extends DefaultHandler {
  // ignore warning, crimson warns even for entity resolution!
  override def warning(ex: SAXParseException): Unit = { }
  override def error(ex: SAXParseException): Unit = printError("Error", ex)
  override def fatalError(ex: SAXParseException): Unit = printError("Fatal Error", ex)

  protected def printError(errtype: String, ex: SAXParseException): Unit =
    Console.withOut(Console.err) {
      val s = "[%s]:%d:%d: %s".format(
        errtype, ex.getLineNumber, ex.getColumnNumber, ex.getMessage)
      Console.println(s)
      Console.flush
    }
}

/** SAX adapter class, for use with Java SAX parser. Keeps track of
 *  namespace bindings, without relying on namespace handling of the
 *  underlying SAX parser.
 */
abstract class FactoryAdapter extends DefaultHandler with factory.XMLLoader[Node] {
  var rootElem: Node = null

  val buffer      = new StringBuilder()
  val attribStack = new mutable.Stack[MetaData]
  val hStack      = new mutable.Stack[Node]   // [ element ] contains siblings
  val tagStack    = new mutable.Stack[String]
  var scopeStack  = new mutable.Stack[NamespaceBinding]

  var curTag : String = null
  var capture: Boolean = false

  // abstract methods

  /** Tests if an XML element contains text.
   * @return true if element named `localName` contains text.
   */
  def nodeContainsText(localName: String): Boolean // abstract

  /** creates an new non-text(tree) node.
   * @param elemName
   * @param attribs
   * @param chIter
   * @return a new XML element.
   */
  def createNode(pre: String, elemName: String, attribs: MetaData,
                 scope: NamespaceBinding, chIter: List[Node]): Node // abstract

  /** creates a Text node.
   * @param text
   * @return a new Text node.
   */
  def createText(text: String): Text // abstract

  /** creates a new processing instruction node.
  */
  def createProcInstr(target: String, data: String): Seq[ProcInstr]

  //
  // ContentHandler methods
  //

  val normalizeWhitespace = false

  /** Characters.
  * @param ch
  * @param offset
  * @param length
  */
  override def characters(ch: Array[Char], offset: Int, length: Int): Unit = {
    if (!capture) return
    // compliant: report every character
    else if (!normalizeWhitespace) buffer.appendAll(ch, offset, length)
    // normalizing whitespace is not compliant, but useful
    else {
      var it = ch.slice(offset, offset + length).iterator
      while (it.hasNext) {
        val c = it.next
        val isSpace = c.isWhitespace
        buffer append (if (isSpace) ' ' else c)
        if (isSpace)
          it = it dropWhile (_.isWhitespace)
      }
    }
  }

  private def splitName(s: String) = {
    val idx = s indexOf ':'
    if (idx < 0) (null, s)
    else (s take idx, s drop (idx + 1))
  }

  /* ContentHandler methods */

  /* Start element. */
  override def startElement(
    uri: String,
    _localName: String,
    qname: String,
    attributes: Attributes): Unit =
  {
    captureText()
    tagStack push curTag
    curTag = qname

    val localName = splitName(qname)._2
    capture = nodeContainsText(localName)

    hStack push null
    var m: MetaData = Null
    var scpe: NamespaceBinding =
      if (scopeStack.isEmpty) TopScope
      else scopeStack.top

    for (i <- 0 until attributes.getLength()) {
      val qname = attributes getQName i
      val value = attributes getValue i
      val (pre, key) = splitName(qname)
      def nullIfEmpty(s: String) = if (s == "") null else s

      if (pre == "xmlns" || (pre == null && qname == "xmlns")) {
        val arg = if (pre == null) null else key
        scpe = new NamespaceBinding(arg, nullIfEmpty(value), scpe)
      }
      else
        m = Attribute(Option(pre), key, Text(value), m)
    }

    scopeStack push scpe
    attribStack push m
  }


  /** captures text, possibly normalizing whitespace
   */
  def captureText(): Unit = {
    if (capture && buffer.length > 0)
      hStack push createText(buffer.toString)

    buffer.clear()
  }

  /** End element.
   * @param uri
   * @param _localName
   * @param qname
   * @throws org.xml.sax.SAXException if ..
   */
  override def endElement(uri: String , _localName: String, qname: String): Unit = {
    captureText()
    val metaData = attribStack.pop

    // reverse order to get it right
    val v = (Iterator continually hStack.pop takeWhile (_ != null)).toList.reverse
    val (pre, localName) = splitName(qname)
    val scp = scopeStack.pop

    // create element
    rootElem = createNode(pre, localName, metaData, scp, v)
    hStack push rootElem
    curTag = tagStack.pop
    capture = curTag != null && nodeContainsText(curTag) // root level
  }

  /** Processing instruction.
  */
  override def processingInstruction(target: String, data: String) {
    hStack pushAll createProcInstr(target, data)
  }
}
