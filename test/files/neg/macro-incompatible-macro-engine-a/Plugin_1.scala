package incompatibleMacroEngine

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin}

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import analyzer._

  val name = "incompatibleMacroEngine"
  val description = "A sample analyzer plugin that crafts a macro impl binding with a non-standard macro engine."
  val components = Nil
  addMacroPlugin(MacroPlugin)

  object MacroPlugin extends MacroPlugin {
    def fixupBinding(tree: Tree) = new Transformer {
      override def transform(tree: Tree) = {
        tree match {
          case Literal(const @ Constant(x)) if tree.tpe == null => tree setType ConstantType(const)
          case _ if tree.tpe == null => tree setType NoType
          case _ => ;
        }
        super.transform(tree)
      }
    }.transform(tree)

    override def pluginsTypedMacroBody(typer: Typer, ddef: DefDef): Option[Tree] = {
      val result = standardTypedMacroBody(typer, ddef)
      val List(AnnotationInfo(atp, List(Apply(nucleus, _ :: others)), Nil)) = ddef.symbol.annotations
      val updatedBinding = Apply(nucleus, Assign(Literal(Constant("macroEngine")), Literal(Constant("vxxx (implemented in the incompatibleMacroEngine plugin)"))) :: others)
      ddef.symbol.setAnnotations(List(AnnotationInfo(atp, List(fixupBinding(updatedBinding)), Nil)))
      Some(result)
    }
  }
}