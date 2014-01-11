import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context): c.Expr[Int] = {
    import c.universe._
    c.Expr(Literal(Constant("42")))
  }
}

object Macros {
  def foo: Int = macro Impls.foo
}