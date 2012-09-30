import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T](c: Ctx)(x: c.Expr[T]): c.Expr[T] = x
}
