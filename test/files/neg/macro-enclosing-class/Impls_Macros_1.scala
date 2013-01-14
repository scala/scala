import scala.reflect.macros.Context
import language.experimental.macros

class C extends scala.annotation.StaticAnnotation
class G[T]

object Macros {
  def impl(c: Context) = {
    c.warning(c.enclosingPosition, s"role = ${c.macroRole}, template = ${c.enclosingImpl.name}")
  }

  def impl1(c: Context) = { impl(c); c.universe.Ident(c.universe.TypeName("C")) }
  def impl2(c: Context)() = { impl(c); c.universe.Ident(c.universe.TypeName("G")) }

  type Foo = macro impl1
  type Bar() = macro impl2
}
