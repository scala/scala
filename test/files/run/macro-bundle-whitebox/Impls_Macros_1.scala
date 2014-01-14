import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class Impl(val c: Context) {
  def mono = { import c.universe._; c.Expr[Unit](q"()") }
  def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](q"${c.weakTypeOf[T].toString}") }
  def weird = macro mono
}

object Macros {
  def mono = macro Impl.mono
  def poly[T] = macro Impl.poly[T]
}

package pkg {
  class Impl(val c: Context) {
    def mono = { import c.universe._; c.Expr[Boolean](q"true") }
    def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](q"${c.weakTypeOf[T].toString + c.weakTypeOf[T].toString}") }
    def weird = macro mono
  }

  object Macros {
    def mono = macro Impl.mono
    def poly[T] = macro Impl.poly[T]
  }
}