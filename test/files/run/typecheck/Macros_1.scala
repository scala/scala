import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    c.typecheck(q"class C")
    q"()"
  }

  def foo: Any = macro impl
}