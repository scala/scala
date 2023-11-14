package example

import scala.language.experimental.macros
import scala.reflect.macros.Context

object Foo {
  def bar(a: Any): Any = macro impl
  def impl(c: Context)(a: c.Expr[Any]): c.Expr[Any] = a
}
