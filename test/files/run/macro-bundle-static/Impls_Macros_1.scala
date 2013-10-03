import scala.reflect.macros.Context
import scala.reflect.macros.Macro
import scala.language.experimental.macros

object Enclosing {
  trait Impl extends Macro {
    def mono = c.literalUnit
    def poly[T: c.WeakTypeTag] = c.literal(c.weakTypeOf[T].toString)
    def weird = macro mono
  }
}

object Macros {
  def mono = macro Enclosing.Impl.mono
  def poly[T] = macro Enclosing.Impl.poly[T]
}

package pkg {
  object Enclosing {
    trait Impl extends Macro {
      def mono = c.literalTrue
      def poly[T: c.WeakTypeTag] = c.literal(c.weakTypeOf[T].toString + c.weakTypeOf[T].toString)
      def weird = macro mono
    }
  }

  object Macros {
    def mono = macro Enclosing.Impl.mono
    def poly[T] = macro Enclosing.Impl.poly[T]
  }
}