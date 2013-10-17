import scala.reflect.macros.Context
import scala.reflect.macros.Macro

trait Impl extends Macro {
  def mono = { import c.universe._; c.Expr[Unit](Literal(Constant(()))) }
  def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](Literal(Constant(c.weakTypeOf[T].toString))) }
  def weird = macro mono
}

object Macros {
  def mono = macro Impl.mono
  def poly[T] = macro Impl.poly[T]
}

package pkg {
  trait Impl extends Macro {
    def mono = { import c.universe._; c.Expr[Boolean](Literal(Constant(true))) }
    def poly[T: c.WeakTypeTag] = { import c.universe._; c.Expr[String](Literal(Constant(c.weakTypeOf[T].toString + c.weakTypeOf[T].toString))) }
    def weird = macro mono
  }

  object Macros {
    def mono = macro Impl.mono
    def poly[T] = macro Impl.poly[T]
  }
}