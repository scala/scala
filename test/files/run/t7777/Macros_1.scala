import scala.language.experimental.macros
import scala.language.dynamics
import scala.reflect.macros.whitebox.Context

class DynMacro extends Dynamic {
  def applyDynamic(s: String)(xs: Any*): DynMacro =
    macro DynMacro.applyDynamicMacro
}

object DynMacro extends DynMacro {
  def applyDynamicMacro(c: Context)(s: c.Expr[String])(xs: c.Expr[Any]*): c.Expr[DynMacro] = {
    import c.universe._
    val Literal(Constant(n: String)) = s.tree
    val args = xs.map(_.tree.toString).mkString("(", ", ", ")")
    c.Expr(q"println(${ n + args }); ${c.prefix.tree}")
  }
}