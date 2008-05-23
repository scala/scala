package plugintemplate

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/** This class shows how to implement a compiler component that
 *  can be used in a compiler plugin.
 *
 *  @todo Adapt the name of this class to the plugin, and implement it.
 */
class TemplateComponent(val global: Global) extends PluginComponent {
  import global._
  import global.definitions._

  val runsAfter = "refchecks"
  /** The phase name of the compiler plugin
   *  @todo Adapt to specific plugin.
   */
  val phaseName = "plugintemplate"

  def newPhase(prev: Phase) = new Phase(prev) {
    def name = phaseName

    /** This method contains the implementation of the compiler
     *  component
     *
     *  @todo Implementation.
     */
    def run {
      println("Hello from phase "+ name)
    }
  }
}
