package macroArgs

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin}

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import analyzer._

  val name = "macroArgs"
  val description = "A sample analyzer plugin that overrides macroArgs."
  val components = Nil
  addMacroPlugin(MacroPlugin)

  object MacroPlugin extends MacroPlugin {
    override def pluginsMacroArgs(typer: Typer, expandee: Tree): Option[MacroArgs] = {
      val MacroArgs(c, List(Literal(Constant(s: String)))) = standardMacroArgs(typer, expandee)
      Some(MacroArgs(c, List(Literal(Constant("hijacked " + s)))))
    }
  }
}