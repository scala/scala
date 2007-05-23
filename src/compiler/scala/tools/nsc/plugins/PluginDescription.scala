package scala.tools.nsc.plugins
import scala.xml.{Node,NodeSeq}
import java.io.File

/** A description of a compiler plugin.
 *
 * @author Lex Spoon
 * @version 1.0, 2007-5-21
 */
abstract class PluginDescription {
  /** A short name of the compiler, used to identify it in
   *  various contexts. The phase defined by the plugin
   *  should have the same name.  */
  val name: String

  /** The name of the main class for the plugin */
  val classname: String

  /** The phase that this plugin should run after.  It can
   *  be the name of another plugin. */
  val runsAfter: String

  /** A one-line description of the plugin, suitable for
   *  a -help listing. */
  val description: Option[String]

  /** A URL to get more information about the plugin */
  val homepage: Option[String]

  /** An XML representation of this description.  It can be
   *  read back using PluginDescription.fromXML . It should
   *  be stored inside the jor.  */
  def toXML: Node = {
    def ifSome(field: Option[String], handler: String=>Node): NodeSeq =
      field match {
	case None => NodeSeq.Empty
	case Some(str) => handler(str)
      }

    <plugin>
      <name>{name}</name>
      <classname>{classname}</classname>
      <after>{runsAfter}</after>
      {ifSome(description, desc =>
	<description>{desc}</description>)}
      {ifSome(homepage, url =>
	<homepage>{url}</homepage>)}
    </plugin>
  }
}



/** Utilities for the PluginDescription class.
 *
 *  @author Lex Spoon
 *  @version 1.0, 2006-5-21
 */
object PluginDescription {
  def fromXML(xml: Node): Option[PluginDescription] = {
    // check the top-level tag
    xml match {
      case <plugin>_*</plugin> => ()
      case _ => None
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
    val runsAfter1 = getField("after") match {
      case None => return None
      case Some(str) => str
    }

    // extract the optional fields
    val homepage1 = getField("homepage")
    val description1 = getField("description")

    Some(new PluginDescription {
      val name = name1
      val classname = classname1
      val runsAfter = runsAfter1
      val homepage = homepage1
      val description = description1
    })
  }
}
