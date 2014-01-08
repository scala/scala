package typedMacroBody

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin}

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import analyzer._

  val name = "typedMacroBody"
  val description = "A sample analyzer plugin that overrides typedMacroBody."
  val components = Nil
  addMacroPlugin(MacroPlugin)

  object MacroPlugin extends MacroPlugin {
    override def pluginsTypedMacroBody(typer: Typer, ddef: DefDef): Option[Tree] = {
      val DefDef(_, _, _, _, _, Literal(Constant(num: Int))) = ddef
      Some(standardTypedMacroBody(typer, copyDefDef(ddef)(rhs = Ident(TermName("impl" + num)))))
    }
  }
}