/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author Lex Spoon
 */

package scala.tools.nsc
package plugins

/*@XML*/
import scala.xml.{Node,NodeSeq}
/*XML@*/
/*@NOXML
import org.w3c.dom.{Node, Document, Text}
import scala.tools.util.XML
XMLNO@*/

/** A description of a compiler plugin, suitable for serialization
 *  to XML for inclusion in the plugin's .jar file.
 *
 * @author Lex Spoon, Stephane Micheloud
 * @version 1.0, 2007-5-21
 */
abstract class PluginDescription {

  /** A short name of the compiler, used to identify it in
   *  various contexts. The phase defined by the plugin
   *  should have the same name.
   */
  val name: String

  /** The name of the main class for the plugin */
  val classname: String

  /** An XML representation of this description.  It can be
   *  read back using <code>PluginDescription.fromXML</code>.
   *  It should be stored inside the jar archive file.
   */
/*@XML*/ // NB. This code DOES rely on Scala native XML support.
  def toXML: Node = {
    <plugin>
      <name>{name}</name>
      <classname>{classname}</classname>
    </plugin>
  }
/*XML@*/
/*@NOXML // NB. This code DOES NOT rely on Scala native XML support.
  def toXML: Node = pluginDoc

  private lazy val pluginDoc: Node = {
    val root = XML.newDocument()

    val pluginElem = root createElement "plugin"
    root appendChild pluginElem

    val nameElem = root createElement "name"
    nameElem appendChild (root createTextNode name)
    pluginElem appendChild nameElem

    val classnameElem = root createElement "classname"
    classnameElem appendChild (root createTextNode classname)
    pluginElem appendChild classnameElem

    root
  }
XMLNO@*/
}

/** Utilities for the PluginDescription class.
 *
 *  @author Lex Spoon, Stephane Micheloud
 *  @version 1.0, 2007-5-21
 */
object PluginDescription {

  def fromXML(xml: Node): Option[PluginDescription] = {
    // check the top-level tag
/*@XML*/
    xml match {
      case <plugin>{_*}</plugin>  => ()
      case _ => return None
    }
/*XML@*/
/*@NOXML
    val node = xml match {
      case root: Document => root.getDocumentElement
      case node => node
    }
    if (node.getNodeName != "plugin")
      return None

    class RichNode(node: Node) {
      def \\(tagName: String): Node = node match {
        case root: Document => root.getElementsByTagName(tagName) item 0
        case _ => node //TODO: visit children
      }
      def text: String = node match {
        case t: Text => t.getWholeText
        case e => e.getTextContent
      }
    }
    implicit def nodeWrapper(node: Node) = new RichNode(node)
XMLNO@*/
    // extract one field
    def getField(field: String): Option[String] = {
      val text = (xml \\ field).text.trim
      if (text == "") None else Some(text)
    }

    // extract the required fields
    val name1 = getField("name") match {
      case None => return None
      case Some(str) => str
    }
    val classname1 = getField("classname") match {
      case None => return None
      case Some(str) => str
    }

    Some(new PluginDescription {
      val name = name1
      val classname = classname1
    })
  }

}
