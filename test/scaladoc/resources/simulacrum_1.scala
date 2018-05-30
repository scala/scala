import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object simulacrumMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    if (annottees.head.tree.getClass.getName.contains("DocDef")) c.abort(c.enclosingPosition, "annottee is a DocDef")
    c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant(()))))
  }
}

class simulacrum extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro simulacrumMacro.impl
}
