import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = x
}
