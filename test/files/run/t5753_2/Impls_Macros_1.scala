import scala.reflect.macros.{Context => Ctx}

trait Macro_T {
 def foo[T](c: Ctx)(s: c.Expr[T]) = s 
}

object Macros {
  def foo[T](s: T) = macro Impls.foo[T]
  object Impls extends Macro_T
}
