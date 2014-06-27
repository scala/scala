package isblackbox

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin}

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import analyzer._
  import scala.reflect.internal.Mode

  val name = "isBlackbox"
  val description = "A sample analyzer plugin that overrides isBlackbox."
  val components = Nil
  addMacroPlugin(MacroPlugin)

  object MacroPlugin extends MacroPlugin {
    override def pluginsIsBlackbox(macroDef: Symbol): Option[Boolean] = {
      Some(false)
    }
  }
}