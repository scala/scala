import language.experimental.macros
import scala.reflect.macros.blackbox.Context

class Impl(val c: Context) {
  def mono = { import c.universe._; c.Expr[Unit](q"()") }
  def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](q"${c.weakTypeOf[T].toString}") }
  def weird: Unit = macro mono
}

object Macros {
  def mono: Unit = macro Impl.mono
  def poly[T]: String = macro Impl.poly[T]
}

package pkg {
  class Impl(val c: Context) {
    def mono = { import c.universe._; c.Expr[Boolean](q"true") }
    def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](q"${c.weakTypeOf[T].toString + c.weakTypeOf[T].toString}") }
    def weird: Boolean = macro mono
  }

  object Macros {
    def mono: Boolean = macro Impl.mono
    def poly[T]: String = macro Impl.poly[T]
  }
}
