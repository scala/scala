package plugintemplate

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/** This class implements a plugin component using a tree
 *  traverser */
class TemplateTraverseComponent (val global: Global) extends PluginComponent {
  import global._
  import global.definitions._

  val runsAfter = List[String]("refchecks")
  /** The phase name of the compiler plugin
   *  @todo Adapt to specific plugin.
   */
  val phaseName = "plugintemplatetraverse"

  def newPhase(prev: Phase): Phase = new TraverserPhase(prev)
  class TraverserPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      newTraverser().traverse(unit.body)
    }
  }

  def newTraverser(): Traverser = new ForeachTreeTraverser(check)

  def check(tree: Tree): Unit = tree match {
    case Apply(fun, args) =>
      println("traversing application of "+ fun)
    case _ => ()
  }
}
