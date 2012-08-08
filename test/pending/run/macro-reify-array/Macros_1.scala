import scala.reflect.macros.{Context => Ctx}

object Macros {
  def foo[T](s: String) = macro Impls.foo[T]

  object Impls {
    def foo[T: c.AbsTypeTag](c: Ctx)(s: c.Expr[T]) = c.universe.reify {
      Array(s.splice)
    }
  }
}