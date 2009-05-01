/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.parsing

import java.io.{InputStream, Reader, File, FileDescriptor, FileInputStream}
import scala.collection.mutable.Stack

import org.xml.sax.{ Attributes, InputSource }
import org.xml.sax.helpers.DefaultHandler
import javax.xml.parsers.{ SAXParser, SAXParserFactory }

// can be mixed into FactoryAdapter if desired
trait ConsoleErrorHandler extends DefaultHandler
{
  import org.xml.sax.SAXParseException

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
abstract class FactoryAdapter extends DefaultHandler() {

  val buffer      = new StringBuilder()
  val attribStack = new Stack[MetaData]
  val hStack      = new Stack[Node]   // [ element ] contains siblings
  val tagStack    = new Stack[String]
  var scopeStack  = new Stack[NamespaceBinding]

  var curTag : String = null
  var capture: Boolean = false

  // abstract methods

  /** Tests if an XML element contains text.
   * @return true if element named <code>localName</code> contains text.
   */
  def nodeContainsText(localName: String): Boolean // abstract

  /** creates an new non-text(tree) node.
   * @param elemName
   * @param attribs
   * @param chIter
   * @return a new XML element.
   */
  def createNode(pre: String, elemName: String, attribs: MetaData,
                 scope: NamespaceBinding, chIter: List[Node]): Node //abstract

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
    if (!normalizeWhitespace) {
      // compliant: report every character
      return buffer.append(ch, offset, length)
    }

    // normalizing whitespace is not compliant, but useful
    var i: Int = offset
    while (i < offset + length) {
      val c = if (ch(i).isWhitespace) ' ' else ch(i)
      buffer append c
      i += 1
      // if that was whitespace, drop until non whitespace
      if (c == ' ') while (ch(i).isWhitespace) i += 1
    }
  }

  /* ContentHandler methods */

  /* Start element. */
  override def startElement(
    uri: String,
    _localName: String,
    qname: String,
    attributes: Attributes): Unit =
  {
    /*elemCount = elemCount + 1; STATISTICS */
    captureText()
    //Console.println("FactoryAdapter::startElement("+uri+","+_localName+","+qname+","+attributes+")");
    tagStack.push(curTag)
    curTag = qname; //localName ;

    val colon = qname.indexOf(':'.asInstanceOf[Int])
    val localName = if(-1 == colon) qname else qname.substring(colon+1,qname.length())

    //Console.println("FactoryAdapter::startElement - localName ="+localName);

    capture = nodeContainsText(localName)

    hStack.push(null)
    var m: MetaData = Null

    var scpe = scopeStack.top
    for (i <- List.range(0, attributes.getLength())) {
      //val attrType = attributes.getType(i); // unused for now
      val qname = attributes.getQName(i)
      val value = attributes.getValue(i)
      val colon = qname.indexOf(':'.asInstanceOf[Int])
      if (-1 != colon) {                     // prefixed attribute
        val pre = qname.substring(0, colon)
        val key = qname.substring(colon+1, qname.length())
        if ("xmlns" /*XML.xmlns*/ == pre)
          scpe = value.length() match {
            case 0 => new NamespaceBinding(key, null,  scpe)
            case _ => new NamespaceBinding(key, value, scpe)
          }
        else
          m = new PrefixedAttribute(pre, key, Text(value), m)
      } else if ("xmlns" /*XML.xmlns*/ == qname)
        scpe = value.length() match {
          case 0 => new NamespaceBinding(null, null,  scpe)
          case _ => new NamespaceBinding(null, value, scpe)
        }
      else
        m = new UnprefixedAttribute(qname, Text(value), m)
    }
    scopeStack.push(scpe)
    attribStack.push(m)
    ()
  } // startElement(String,String,String,Attributes)


  /** captures text, possibly normalizing whitespace
   */
  def captureText(): Unit = {
    if (capture) {
      val text = buffer.toString()
      if (text.length() > 0)
        hStack.push(createText(text))
    }
    buffer.setLength(0)
  }

  /** End element.
   * @param uri
   * @param localName
   * @param qname
   * @throws org.xml.sax.SAXException if ..
   */
  override def endElement(uri: String , _localName: String , qname: String): Unit = {
    captureText()

    val metaData = attribStack.pop

    // reverse order to get it right
    var v: List[Node] = Nil
    var child: Node = hStack.pop
    while (child ne null) {
      v = child::v
      child = hStack.pop
    }

    val colon = qname.indexOf(':'.asInstanceOf[Int])
    val localName =
      if (-1 == colon) qname
      else qname.substring(colon+1, qname.length())

    val scp = scopeStack.pop
    // create element
    val pre = if (-1 == colon) null else qname.substring(0, colon)
    rootElem = createNode(pre, localName, metaData, scp, v)

    hStack.push(rootElem)

    // set
    curTag = tagStack.pop

    capture =
      if (curTag ne null) nodeContainsText(curTag) // root level
      else false
  } // endElement(String,String,String)

  /** Processing instruction.
  */
  override def processingInstruction(target: String, data: String) {
    for (pi <- createProcInstr(target, data))
      hStack.push(pi)
  }

  var rootElem: Node = null

  //FactoryAdapter
  // MAIN
  //

  private def mkParser(): SAXParser = {
    val f = SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    f.newSAXParser()
  }

  /** load XML document
   * @param source
   * @return a new XML document object
   */
  def loadXML(source: InputSource): Node = {
    val parser: SAXParser = mkParser
    scopeStack.push(TopScope)
    parser.parse(source, this)
    scopeStack.pop

    rootElem
  } // loadXML

  /** loads XML from given file */
  def loadFile(file: File): Node =
    loadXML(new InputSource(new FileInputStream(file)))

  /** loads XML from given file descriptor */
  def loadFile(fileDesc: FileDescriptor): Node =
    loadXML(new InputSource(new FileInputStream(fileDesc)))

  /** loads XML from given file */
  def loadFile(fileName: String): Node =
    loadXML(new InputSource(new FileInputStream(fileName)))

  /** loads XML from given InputStream */
  def load(is: InputStream): Node =
    loadXML(new InputSource(is))

  /** loads XML from given Reader */
  def load(reader: Reader): Node =
    loadXML(new InputSource(reader))

  /** loads XML from given sysID */
  def load(sysID: String): Node =
    loadXML(new InputSource(sysID))

}
