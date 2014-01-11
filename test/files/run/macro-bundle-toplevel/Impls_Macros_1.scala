import scala.reflect.macros.blackbox.Macro

trait Impl extends Macro {
  def mono = { import c.universe._; c.Expr[Unit](q"()") }
  def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](q"${c.weakTypeOf[T].toString}") }
  def weird = macro mono
}

object Macros {
  def mono = macro Impl.mono
  def poly[T] = macro Impl.poly[T]
}

package pkg {
  trait Impl extends Macro {
    def mono = { import c.universe._; c.Expr[Boolean](q"true") }
    def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](q"${c.weakTypeOf[T].toString + c.weakTypeOf[T].toString}") }
    def weird = macro mono
  }

  object Macros {
    def mono = macro Impl.mono
    def poly[T] = macro Impl.poly[T]
  }
}