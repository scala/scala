import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Macros {
  def foo[T](s: T) = macro Impls.foo[T]

  object Impls {
    def foo[T: c.WeakTypeTag](c: Ctx)(s: c.Expr[T]) = c.universe.reify {
      List(s.splice)
    }
  }
}