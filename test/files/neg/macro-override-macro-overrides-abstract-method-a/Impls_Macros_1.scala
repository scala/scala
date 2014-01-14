import scala.reflect.macros.blackbox.Context

object Impls {
  def impl(c: Context)(x: c.Expr[Int]) = x
}

trait Foo {
  def foo(x: Int): Int
}

object Macros extends Foo {
  def foo(x: Int): Int = macro Impls.impl
}
