package macroRuntime

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin}

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import analyzer._

  val name = "macroRuntime"
  val description = "A sample analyzer plugin that overrides macroRuntime."
  val components = Nil
  addMacroPlugin(MacroPlugin)

  object MacroPlugin extends MacroPlugin {
    override def pluginsMacroRuntime(expandee: Tree): Option[MacroRuntime] = Some({
      case MacroArgs(_, List(msg)) => q"""println("hijacked")"""
    })
  }
}