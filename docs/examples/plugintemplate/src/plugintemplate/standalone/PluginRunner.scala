package plugintemplate.standalone

import scala.tools.nsc.{Global, Settings, SubComponent}
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}

/** This class is a compiler that will be used for running
 *  the plugin in standalone mode.
 */
class PluginRunner(settings: Settings, reporter: Reporter)
extends Global(settings, reporter) {
  def this(settings: Settings) = this(settings, new ConsoleReporter(settings))

  val annotChecker = new TemplateAnnotationChecker {
      val global: PluginRunner.this.type = PluginRunner.this
  }
  addAnnotationChecker(annotChecker.checker)

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
