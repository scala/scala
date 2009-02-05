package plugintemplate

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/** This class shows how to implement a compiler component that
 *  can be used in a compiler plugin. If the plugin uses a tree
 *  transformer and / or an InfoTransformer, look at the two
 *  classes <code>TemplateTransformComponent</code> and
 *  <code>TemplateInfoTransformComponent</code>.
 *
 *  @todo Adapt the name of this class to the plugin, and implement it.
 */
class TemplateComponent(val global: Global) extends PluginComponent {
  import global._

  val runsAfter = List[String]("refchecks")

  /** The name of this plugin phase
   *  @todo Adapt to specific plugin.
   */
  val phaseName = "plugintemplate"

  def newPhase(prev: Phase) = new Phase(prev) {
    def name = phaseName

    /** The implementation of this Phase's behavior
     *
     *  @todo Implementation.
     */
    def run {
      println("Hello from phase "+ name)
    }
  }
}
