import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

object Enclosing {
  class Impl(val c: Context) {
    def mono = { import c.universe._; c.Expr[Unit](q"()") }
    def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](q"${c.weakTypeOf[T].toString}") }
    def weird = macro mono
  }
}

object Macros {
  def mono = macro Enclosing.Impl.mono
  def poly[T] = macro Enclosing.Impl.poly[T]
}

package pkg {
  object Enclosing {
    class Impl(val c: Context) {
      def mono = { import c.universe._; c.Expr[Boolean](q"true") }
      def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](q"${c.weakTypeOf[T].toString + c.weakTypeOf[T].toString}") }
      def weird = macro mono
    }
  }

  object Macros {
    def mono = macro Enclosing.Impl.mono
    def poly[T] = macro Enclosing.Impl.poly[T]
  }
}