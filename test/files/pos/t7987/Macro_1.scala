import scala.language.experimental._

object Macro {
  def apply[A](a: A): A = macro impl[A]
  def impl[A](c: reflect.macros.Context)(a: c.Expr[A]): c.Expr[A] = a
}
