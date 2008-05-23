package plugintemplate

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin

/** A class describing the compiler plugin
 *
 *  @todo Adapt the name of this class to the plugin being
 *  implemented
 */
class TemplatePlugin(val global: Global) extends Plugin {
  /** The name of this plugin. Extracted from the properties file. */
  val name = PluginProperties.pluginName

  /** A short description of the plugin, read from the properties file */
  val description = PluginProperties.pluginDescription

  /** The compiler components that will be applied when running
   *  this plugin
   *
   *  @todo Adapt to the plugin being implemented
   */
  val components=List(new TemplateComponent(global))
}
