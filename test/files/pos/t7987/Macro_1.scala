import scala.language.experimental.macros

object Macro {
  def apply[A](a: A): A = macro impl[A]
  def impl[A](c: reflect.macros.Context)(a: c.Expr[A]): c.Expr[A] = a
}
