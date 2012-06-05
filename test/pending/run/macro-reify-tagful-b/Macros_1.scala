import scala.reflect.makro.{Context => Ctx}

object Macros {
  def foo[T](s: T) = macro Impls.foo[List[T]]

  object Impls {
    def foo[T: c.TypeTag](c: Ctx)(s: c.Expr[T]) = c.reify {
      List(s.splice)
    }
  }
}