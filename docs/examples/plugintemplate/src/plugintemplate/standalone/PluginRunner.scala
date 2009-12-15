package plugintemplate.standalone

import plugintemplate.{TemplateAnnotationChecker, TemplatePlugin}
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
  override protected def computeInternalPhases() {
    phasesSet += syntaxAnalyzer
    phasesSet += analyzer.namerFactory
    phasesSet += analyzer.typerFactory
    phasesSet += superAccessors			       // add super accessors
    phasesSet += pickler			       // serialize symbol tables
    phasesSet += refchecks			       // perform reference and override checking, translate nested objects

    for (phase <- TemplatePlugin.components(this)) {
      phasesSet += phase
    }
  }

}
