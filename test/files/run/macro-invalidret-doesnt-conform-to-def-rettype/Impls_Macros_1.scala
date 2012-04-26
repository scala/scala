import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx): c.Expr[Int] = {
    import c.mirror._
    Expr(Literal(Constant("42")))
  }
}

object Macros {
  def foo: Int = macro Impls.foo
}