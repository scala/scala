import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

object Enclosing {
  class Impl(val c: Context) {
    def mono = { import c.universe._; c.Expr[Unit](q"()") }
    def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](q"${c.weakTypeOf[T].toString}") }
    def weird: Unit = macro mono
  }
}

object Macros {
  def mono: Unit = macro Enclosing.Impl.mono
  def poly[T]: String = macro Enclosing.Impl.poly[T]
}

package pkg {
  object Enclosing {
    class Impl(val c: Context) {
      def mono = { import c.universe._; c.Expr[Boolean](q"true") }
      def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](q"${c.weakTypeOf[T].toString + c.weakTypeOf[T].toString}") }
      def weird: Boolean = macro mono
    }
  }

  object Macros {
    def mono: Boolean = macro Enclosing.Impl.mono
    def poly[T]: String = macro Enclosing.Impl.poly[T]
  }
}
