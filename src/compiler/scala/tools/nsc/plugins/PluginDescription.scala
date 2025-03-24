/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package plugins

/** A description of a compiler plugin, suitable for serialization
 *  to XML for inclusion in the plugin's .jar file.
 *
 * @author Lex Spoon
 * @author Adriaan Moors
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
