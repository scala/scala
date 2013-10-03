import scala.reflect.macros.Context
import scala.reflect.macros.Macro

trait Impl extends Macro {
  def mono = c.literalUnit
  def poly[T: c.WeakTypeTag] = c.literal(c.weakTypeOf[T].toString)
  def weird = macro mono
}

object Macros {
  def mono = macro Impl.mono
  def poly[T] = macro Impl.poly[T]
}

package pkg {
  trait Impl extends Macro {
    def mono = c.literalTrue
    def poly[T: c.WeakTypeTag] = c.literal(c.weakTypeOf[T].toString + c.weakTypeOf[T].toString)
    def weird = macro mono
  }

  object Macros {
    def mono = macro Impl.mono
    def poly[T] = macro Impl.poly[T]
  }
}