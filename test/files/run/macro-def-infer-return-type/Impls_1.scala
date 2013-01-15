import scala.reflect.macros.Context

object Impls1 {
  def foo(c: Context)(x: c.Expr[Int]) = x
}

object Impls2 {
  def foo[T](c: Context)(x: c.Expr[T]) =
    throw new Error("an implementation is missing")
}

object Impls3 {
  def foo[T](c: Context)(x: c.Expr[T]): c.Expr[T] = x
}
