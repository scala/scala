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
  val components = TemplatePlugin.components(global)
  val runsAfter = "refchecks"

  /* TODO: include annotationChecker
  import global._

  println("adding annotationchecker...")
  addAnnotationChecker(new AnnotationChecker {
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      println("checking: "+ tpe1 +" <: "+ tpe2)
      true
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      //println("adding annot to "+ tree.symbol)
      tpe
    }
  })
  */
}

object TemplatePlugin {
  /** Yields the list of Components to be executed in this plugin
   *
   *  @todo: Adapt to specific implementation.
   */
  def components(global: Global) =
    List(new TemplateComponent(global),
         new TemplateTransformComponent(global),
         new TemplateInfoTransformComponent(global))
}
