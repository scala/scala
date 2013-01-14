import scala.reflect.macros.Context

object Macros {
  def impl1(c: Context)(x: c.Tree) = ???
  def foo1(x: _) = macro impl1

  def impl2(c: Context)(x: c.Tree*) = ???
  def foo2(x: _*) = macro impl2

  def impl3(c: Context) = ???
  def foo3: _ = macro impl3

  def impl4(c: Context) = ???
  def foo4: _* = macro impl4
}