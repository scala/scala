import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[Int]) = {
    import c.universe._
    val body = Apply(Select(x.tree, TermName("$plus")), List(Literal(Constant(1))))
    c.Expr[Int](body)
  }
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}