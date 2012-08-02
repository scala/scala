import scala.reflect.macros.{Context => Ctx}

object Macros {
  def foo[T](s: T) = macro Impls.foo[List[T]]

  object Impls {
    def foo[T](c: Ctx)(s: c.Expr[T]) = c.universe.reify {
      List(s.splice)
    }
  }
}