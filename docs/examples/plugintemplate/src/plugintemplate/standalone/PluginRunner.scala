package plugintemplate.standalone

import scala.tools.nsc.{Global, Settings, SubComponent}
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}

/** This class is a compiler that will be used for running
 *  the plugin in standalone mode.
 *
 *  @todo Adapt to specific plugin.
 */
class PluginRunner(settings: Settings, reporter: Reporter)
extends Global(settings, reporter) {
  def this(settings: Settings) = this(settings, new ConsoleReporter(settings))

  /** The plugin component that should will executed.
   *
   *  @todo Adapt to specific plugin. It is possible to add multiple
   *  plugin components to run.
   */
  val pluginComponent = new TemplateComponent(this)

  override def phaseDescriptors: List[SubComponent] = List(
    analyzer.namerFactory,
    analyzer.typerFactory,
    superAccessors,
    pickler,
    refchecks,
    pluginComponent
  )
}
