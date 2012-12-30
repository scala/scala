import scala.reflect.macros.Context

object Statistics {
  var timesExpanded = 0
}

class C extends scala.annotation.StaticAnnotation
class HK[T]

object Macros {
  def impl_foo(c: Context) = { Statistics.timesExpanded += 1; c.universe.Ident(c.universe.TypeName("C")) }
  type Foo = macro impl_foo

  def impl_hk(c: Context)() = { Statistics.timesExpanded += 1; c.universe.Ident(c.universe.TypeName("HK")) }
  type FooHK() = macro impl_hk

  def impl(c: Context) = c.literal(Statistics.timesExpanded)
  def timesExpanded = macro impl
}