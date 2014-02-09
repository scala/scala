import language.experimental._
import scala.reflect.macros.blackbox.Context

object M {
  def noopImpl[A](c: Context)(expr: c.Expr[A]): c.Expr[A] = c.Expr(c.typecheck(c.untypecheck(expr.tree)))
  def noop[A](expr: A): A = macro noopImpl[A]
}
