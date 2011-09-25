/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stephane Micheloud
 */

package scala.tools
package util

/** The object `XML` provides minimal XML support for creating, loading
 *  and saving XML documents (see eg. [[scala.tools.ant.ScalaBazaar]],
 *  [[scala.tools.nsc.plugins.PluginDescription]] and
 *  [[scala.tools.partest.PartestTask]]).
 *
 *  It makes possible for the programmer of Scala tools not to rely on the
 *  XML features of the reference implementation of the Scala compiler
 *  (some Scala customers may for instance not be interested in that features).
 *
 *  @author  Stephane Micheloud
 *  @version 1.0
 */
/*@NOXML
// [mics] used in code which DOES NOT rely on Scala native XML support
// (see eg. classes partest/PartestTask.scala, ant/ScalaBazaar.scala).
object XML {
  import java.io.{FileOutputStream, InputStream, Writer}
  import java.nio.channels.Channels
  import javax.xml.parsers.DocumentBuilderFactory
  import org.w3c.dom.{Document, DocumentType, Element, NamedNodeMap}
  import org.w3c.dom.{Node => JNode, Text => JText}
  import org.xml.sax.InputSource
  import scala.util.control.Exception.ultimately

  def newDocument(): Document = newBuilder.newDocument()

  def loadXML(source: InputSource): Document = newBuilder parse source

  def load(in: InputStream) = loadXML(new InputSource(in))

  final def save(filename: String, node: Node,
                 encoding: String = "ISO-8859-1",
                 xmlDecl: Boolean = false,
                 doctype: DocumentType = null) {
    val fos = new FileOutputStream(filename)
    val w = Channels.newWriter(fos.getChannel, encoding)

    ultimately(w.close()) {
      write(w, node, encoding, xmlDecl, doctype)
    }
  }

  final def write(out: Writer, node: Node, encoding: String, xmlDecl: Boolean, doctype: DocumentType) {
    if (xmlDecl) out.write("<?xml version='1.0' encoding='" + encoding + "'?>\n")
    if (doctype ne null) out.write(doctype.getName + "\n")
    out write node.toXMLString
  }

  class Node(val node: JNode) {
    def toXMLString: String = {
      var indent: Int = 0
      val sb = new StringBuilder()
      def xmlTag(s: String, attrs: NamedNodeMap, trail: String) {
        var i = 0; while (i < indent) { sb append spaces; i += 1 }
        sb append "<" append s
        for (i <- 0 until attrs.getLength) {
          val attr = attrs item i
          sb append " " append attr.getNodeName append "=\"" append attr.getTextContent append "\""
        }
        sb append trail
      }
      def startTag(s: String, attrs: NamedNodeMap) {
        xmlTag(s, attrs, Node.TAG_TRAIL_EOL)
        indent += 1
      }
      def startTag1(s: String, attrs: NamedNodeMap) {
        xmlTag(s, attrs, Node.TAG_TRAIL)
      }
      def shortTag(s: String, attrs: NamedNodeMap) {
        xmlTag(s, attrs, Node.STAG_TRAIL_EOL)
      }
      def endTag(s: String) {
        indent -= 1
        var i = 0; while (i < indent) { sb append spaces; i += 1 }
        sb append "</" append s append Node.TAG_TRAIL_EOL
      }
      def endTag1(s: String) {
        sb append "</" append s append Node.TAG_TRAIL_EOL
      }
      def traverse(node: JNode) {
        val name = node.getNodeName
        val attrs = node.getAttributes
        var children = node.getChildNodes
        val n = children.getLength
        if (n == 1 && children.item(0).isInstanceOf[JText]) {
          startTag1(name, attrs)
          sb append children.item(0).asInstanceOf[JText].getWholeText
          endTag1(name)
        }
        else if (n > 0) {
          startTag(name, attrs)
          for (i <- 0 until n) {
            mkString(children item i)
          }
          endTag(name)
        }
        else
          shortTag(name, attrs)
      }
      def mkString(node: JNode) = node match {
        case t: JText => sb append t.getWholeText
        case e => traverse(e)
      }
	  traverse(node match {
	    case docu: Document => docu.getDocumentElement
		case elem => elem
	  })
      sb.toString
    }

    def text: String = node match {
      case t: JText => t.getWholeText
      case n => n.getTextContent
    }

    override def toString: String = toXMLString
  }

  implicit def nodeWrapper(node: JNode) = new Node(node)

  // ---------- private declarations --------

  private val docFactory = DocumentBuilderFactory.newInstance()
  docFactory setNamespaceAware false
  private def newBuilder = docFactory.newDocumentBuilder()

  private var spaces = "  "
  def spaces_=(n: Int) { spaces = List.fill(n)(' ').mkString }

  private object Node {
    final val TAG_TRAIL = ">"
    final val TAG_TRAIL_EOL = TAG_TRAIL+compat.Platform.EOL
    final val STAG_TRAIL = "/>"
    final val STAG_TRAIL_EOL = STAG_TRAIL+compat.Platform.EOL
  }

}
XMLNO@*/

