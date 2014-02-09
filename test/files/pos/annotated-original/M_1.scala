import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object M {
  def impl(c: Context)(a: c.Expr[Any]) = c.Expr[Any](c.untypecheck(a.tree))
  def m(a: Any) = macro impl
}
