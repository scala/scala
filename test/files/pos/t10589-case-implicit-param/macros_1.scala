//> using options -Ymacro-annotations

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class macid extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macidMacro.impl
}
object macidMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    new Macros[c.type](c).macidMacroImpl(annottees.toList)
  }
}
class Macros[C <: Context](val c: C) {
  import c.universe._
  def macidMacroImpl(annottees: List[c.Expr[Any]]): c.Expr[Any] =
    annottees(0)
}
