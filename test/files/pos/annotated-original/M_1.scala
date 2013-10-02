import language.experimental.macros
import reflect.macros.Context

object M {
  def impl(c: Context)(a: c.Expr[Any]) = c.Expr[Any](c.resetLocalAttrs(a.tree))
  def m(a: Any) = macro impl
}
