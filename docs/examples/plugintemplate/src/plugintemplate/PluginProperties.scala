package plugintemplate

import java.util.Properties

/** A utility to load properties of this plugin via the property
 *  file "plugin.properties"
 */
object PluginProperties {
  private val propFilename = "plugin.properties"

  val pluginName = getOrElse("plugin.name", "(name_unknown)")
  val pluginDescription = getOrElse("plugin.description", "(plugin description not found)")
  val pluginCommand = getOrElse("plugin.commandname", "(command_unknown)")
  val versionString = {
    val default = "(version_unknown)"
    props match {
      case Some(p) =>
        val major = p.getProperty("version.major")
        val minor = p.getProperty("version.minor")
        if ((major eq null) || (minor eq null)) default
        else major +"."+ minor
      case None => default
    }
  }

  private def getOrElse(property: String, default: String) = {
    props match {
      case Some(p) if (p.getProperty(property) != null) =>
        p.getProperty(property)
      case _ =>
        default
    }
  }

  private lazy val props: Option[Properties] = {
    /** Running from JAR file: the properties file should be in the
     *  jar as well
     */
    var stream = this.getClass.getResourceAsStream("/"+ propFilename)
    if (stream == null) {
      /** Running from .class files: expect classfiles to be in
       *  directory [...]/build/build.main, and [...] to contain
       *  the properties file.
       */
      try {
        val current = this.getClass.getClassLoader.getResource(".")
        val dir = new java.io.File(current.toURI)
        // dir will be [...]/build/build.main/
        stream = new java.io.FileInputStream(dir.getParentFile.getParent +"/"+ propFilename)
      } catch {
        case _ => ()
      }
    }
    if (stream == null) None
    else {
      val p = new Properties
      p.load(stream)
      Some(p)
    }
  }
}
