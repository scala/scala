import scala.reflect.macros.blackbox.Context

class Impls1 {
  def foo(c: Context)(x: c.Expr[Any]) = ???
}

object Impls2 {
  def foo(c: Context)(x: c.Expr[Any]) = ???
}

trait MacroHelpers {
  object Impls4 {
    def foo(c: Context)(x: c.Expr[Any]) = x
  }
}

object Impls5 {
  def foo(c: Context)(x: c.Expr[Any]) = ???
  def foo(c: Context)(x: c.Expr[Any], y: c.Expr[Any]) = ???
}

object Impls6 {
  def fooNullary(c: Context) = {
    import c.universe._
    c.Expr[Unit](q"""Predef.println("it works")""")
  }

  def fooEmpty(c: Context)() = fooNullary(c)
}

object Impls7 {
  def foo[U <: Int](c: Context) = ???
}

package foo {
  object Impls8 {
    private[foo] def impl(c: Context) = ???
  }
}