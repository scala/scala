import scala.reflect.macros.Context

object Macros {
  def impl1(c: Context)(x: c.Tree) = ???
  type Foo1(x: _) = macro impl1

  def impl2(c: Context)(x: c.Tree*) = ???
  type Foo2(x: _*) = macro impl2
}