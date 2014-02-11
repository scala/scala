import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.blackbox.Context

class A
object A { implicit def a2b(a: A): B = ??? }
class B
class C extends A

object Macros {
  def impl(c: Context) = {
    import c.universe._
    q"new C"
  }

  def foo: A = macro impl
}