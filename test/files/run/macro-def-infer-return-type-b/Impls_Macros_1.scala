import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T](c: Ctx)(x: c.Expr[T]) =
    throw new Error("an implementation is missing")
}

object Macros {
  def foo[T](x: T) = macro Impls.foo[T]
}