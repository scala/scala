import scala.reflect.macros.BlackboxContext

class Impls1 {
  def foo(c: BlackboxContext)(x: c.Expr[Any]) = ???
}

object Impls2 {
  def foo(c: BlackboxContext)(x: c.Expr[Any]) = ???
}

trait MacroHelpers {
  object Impls4 {
    def foo(c: BlackboxContext)(x: c.Expr[Any]) = x
  }
}

object Impls5 {
  def foo(c: BlackboxContext)(x: c.Expr[Any]) = ???
  def foo(c: BlackboxContext)(x: c.Expr[Any], y: c.Expr[Any]) = ???
}

object Impls6 {
  def fooNullary(c: BlackboxContext) = {
    import c.universe._
    c.Expr[Unit](q"""Predef.println("it works")""")
  }

  def fooEmpty(c: BlackboxContext)() = fooNullary(c)
}

object Impls7 {
  def foo[U <: Int](c: BlackboxContext) = ???
}

package foo {
  object Impls8 {
    private[foo] def impl(c: BlackboxContext) = ???
  }
}