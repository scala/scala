import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx): c.Expr[Int] = {
    import c.universe._
    c.Expr(Literal(Constant("42")))
  }
}

object Macros {
  def foo: Int = macro Impls.foo
}