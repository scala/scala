/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author Lex Spoon
 */

package scala.tools.nsc
package plugins

import java.io.File

import scala.xml.{Node,NodeSeq}

/** A description of a compiler plugin, suitable for serialization
 *  to XML for inclusion in the plugin's .jar file.
 *
 * @author Lex Spoon
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
   *  It should be stored inside the jar.
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
  def fromXML(xml: Node): Option[PluginDescription] = {
    // check the top-level tag
    xml match {
      case <plugin>{_*}</plugin>  => ()
      case _ => return None
    }

    /** Extract one field */
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
