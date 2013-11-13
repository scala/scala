import scala.reflect.macros.BlackboxContext

object Impls1 {
  def foo(c: BlackboxContext)(x: c.Expr[Int]) = x
}

object Impls2 {
  def foo[T](c: BlackboxContext)(x: c.Expr[T]) =
    throw new Error("an implementation is missing")
}

object Impls3 {
  def foo[T](c: BlackboxContext)(x: c.Expr[T]): c.Expr[T] = x
}
