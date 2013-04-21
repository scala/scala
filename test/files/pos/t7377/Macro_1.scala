import language.experimental._
import reflect.macros.Context

object M {
  def noopImpl[A](c: Context)(expr: c.Expr[A]): c.Expr[A] = c.Expr(c.typeCheck(c.resetLocalAttrs(expr.tree)))
  def noop[A](expr: A): A = macro noopImpl[A]
}
