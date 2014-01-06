import language.experimental.macros
import reflect.macros.BlackboxContext

object M {
  def impl(c: BlackboxContext)(a: c.Expr[Any]) = c.Expr[Any](c.resetLocalAttrs(a.tree))
  def m(a: Any) = macro impl
}
