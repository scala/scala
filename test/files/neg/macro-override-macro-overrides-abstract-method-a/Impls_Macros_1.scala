import scala.reflect.macros.{BlackboxContext => Ctx}

object Impls {
  def impl(c: Ctx)(x: c.Expr[Int]) = x
}

trait Foo {
  def foo(x: Int): Int
}

object Macros extends Foo {
  def foo(x: Int): Int = macro Impls.impl
}
