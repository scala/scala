package macroExpand

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin}

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import analyzer._
  import scala.reflect.internal.Mode

  val name = "macroExpand"
  val description = "A sample analyzer plugin that overrides macroExpand."
  val components = Nil
  addMacroPlugin(MacroPlugin)

  object MacroPlugin extends MacroPlugin {
    override def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = {
      object expander extends DefMacroExpander(typer, expandee, mode, pt) {
        override def onSuccess(expanded: Tree) = {
          val message = s"expanded into ${expanded.toString}"
          typer.typed(q"println($message)")
        }
      }
      Some(expander(expandee))
    }
  }
}