/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author Lex Spoon
 */

package scala.tools.nsc
package plugins

import scala.xml.Node

/** A description of a compiler plugin, suitable for serialization
 *  to XML for inclusion in the plugin's .jar file.
 *
 * @author Lex Spoon
 * @version 1.0, 2007-5-21
 *  @param name A short name of the plugin, used to identify it in
 *    various contexts. The phase defined by the plugin
 *    should have the same name.
 *  @param classname The name of the main Plugin class.
 */
case class PluginDescription(name: String, classname: String) {

  /** An XML representation of this description.  It can be
   *  read back using `PluginDescription.fromXML`.
   *  It should be stored inside the jar archive file.
   */
  def toXML: Node = {
    <plugin>
      <name>{name}</name>
      <classname>{classname}</classname>
    </plugin>
  }
}

/** Utilities for the PluginDescription class.
 *
 *  @author Lex Spoon
 *  @version 1.0, 2007-5-21
 */
object PluginDescription {

  def fromXML(xml: Node): PluginDescription = {
    // extract one field
    def getField(field: String): Option[String] = {
      val text = (xml \\ field).text.trim
      if (text == "") None else Some(text)
    }
    def extracted = {
      val name = "name"
      val claas = "classname"
      val vs = Map(name -> getField(name), claas -> getField(claas))
      if (vs.values exists (_.isEmpty)) fail()
      else PluginDescription(name = vs(name).get, classname = vs(claas).get)
    }
    def fail() = throw new RuntimeException("Bad plugin descriptor.")
    // check the top-level tag
    xml match {
      case <plugin>{_*}</plugin>  => extracted
      case _                      => fail()
    }
  }
}
