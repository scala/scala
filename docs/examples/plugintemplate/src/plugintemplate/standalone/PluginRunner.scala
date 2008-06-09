package plugintemplate.standalone

import scala.tools.nsc.{Global, Settings, SubComponent}
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}

/** This class is a compiler that will be used for running
 *  the plugin in standalone mode.
 */
class PluginRunner(settings: Settings, reporter: Reporter)
extends Global(settings, reporter) {
  def this(settings: Settings) = this(settings, new ConsoleReporter(settings))

  /* TODO: include AnnotationChecker
  println("adding annotationchecker...")
  addAnnotationChecker(new AnnotationChecker {
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      def getAnnTpe(t: Type) = t match {
        case AnnotatedType(attrs, underlying, selfsym) =>
          attrs match {
            case x :: xs => Some(x.atp)
            case _ => None
          }
        case _ => None
      }
      val ta1 = getAnnTpe(tpe1)
      val ta2 = getAnnTpe(tpe2)
      ta1 == ta2
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      //println("adding annot to "+ tree.symbol)
      tpe
    }
  })
  */

  /** The phases to be run.
   *
   *  @todo: Adapt to specific plugin implementation
   */
  override def phaseDescriptors: List[SubComponent] = List(
    analyzer.namerFactory,
    analyzer.typerFactory,
    superAccessors,
    pickler,
    refchecks) ::: TemplatePlugin.components(this)
}
