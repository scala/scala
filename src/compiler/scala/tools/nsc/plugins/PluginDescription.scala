/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author Lex Spoon
 */

package scala.tools.nsc
package plugins

import scala.reflect.internal.util.StringContextStripMarginOps

/** A description of a compiler plugin, suitable for serialization
 *  to XML for inclusion in the plugin's .jar file.
 *
 * @author Lex Spoon
 * @version 1.0, 2007-5-21
 * @author Adriaan Moors
 * @version 2.0, 2013
 * @param name A short name of the plugin, used to identify it in
 *   various contexts. The phase defined by the plugin
 *   should have the same name.
 * @param classname The name of the main Plugin class.
 */
case class PluginDescription(name: String, classname: String) {
  /** An XML representation of this description.
   *  It should be stored inside the jar archive file.
   */
  def toXML: String =
    sm"""<plugin>
         | <name>${name}</name>
         | <classname>${classname}</classname>
         |</plugin>"""
}

/** Utilities for the PluginDescription class.
 *
 * @author Lex Spoon
 * @version 1.0, 2007-5-21
 * @author Adriaan Moors
 * @version 2.0, 2013
 */
object PluginDescription {
  private def text(ns: org.w3c.dom.NodeList): String =
    if (ns.getLength == 1) ns.item(0).getTextContent.trim
    else throw new RuntimeException("Bad plugin descriptor.")

  def fromXML(xml: java.io.InputStream): PluginDescription = {
    import javax.xml.parsers.DocumentBuilderFactory
    val root = DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(xml).getDocumentElement
    root.normalize()
    if (root.getNodeName != "plugin")
      throw new RuntimeException("Plugin descriptor root element must be <plugin>.")

    PluginDescription(text(root.getElementsByTagName("name")), text(root.getElementsByTagName("classname")))
  }
}
