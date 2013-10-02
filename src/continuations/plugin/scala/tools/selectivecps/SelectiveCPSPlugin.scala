// $Id$

package scala.tools.selectivecps

import scala.tools.nsc
import nsc.Global
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class SelectiveCPSPlugin(val global: Global) extends Plugin {
  val name = "continuations"
  val description = "applies selective cps conversion"

  val pluginEnabled = options contains "enable"

  val anfPhase = new {
    val global = SelectiveCPSPlugin.this.global
    val cpsEnabled = pluginEnabled
    override val enabled = cpsEnabled
  } with SelectiveANFTransform {
    val runsAfter = List("pickler")
  }

  val cpsPhase = new {
    val global = SelectiveCPSPlugin.this.global
    val cpsEnabled = pluginEnabled
    override val enabled = cpsEnabled
  } with SelectiveCPSTransform {
    val runsAfter = List("selectiveanf")
    override val runsBefore = List("uncurry")
  }

  val components = List[PluginComponent](anfPhase, cpsPhase)

  val checker = new {
    val global: SelectiveCPSPlugin.this.global.type = SelectiveCPSPlugin.this.global
    val cpsEnabled = pluginEnabled
  } with CPSAnnotationChecker

  // TODO don't muck up global with unused checkers
  global.addAnnotationChecker(checker.checker)
  global.analyzer.addAnalyzerPlugin(checker.plugin)

  global.log("instantiated cps plugin: " + this)

  override def init(options: List[String], error: String => Unit) = {
    options foreach {
      case "enable" => // in initializer
      case arg      => error(s"Bad argument: $arg")
    }
    pluginEnabled
  }

  override val optionsHelp: Option[String] =
    Some("  -P:continuations:enable        Enable continuations")
}
