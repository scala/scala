import scala.language.experimental.macros

import scala.reflect.macros.whitebox.Context

final class TwoFaceInt[T](val value : Int) {
  def + [R](that : TwoFaceInt[R]) = ???
}
object TwoFaceInt {
  def apply[T <: Int, Out <: T](value : T) : TwoFaceInt[Out] = macro Builder.Macro.fromNumValue[T]
}

object Builder {
  final class Macro(val c: Context) {
    def fromNumValue[T](value : c.Tree)(implicit t : c.WeakTypeTag[T]) : c.Tree = {
      import c.universe._
      val tTpe = weakTypeOf[T]
      val valueTpe = value match {
        case Literal(Constant(t : Int)) => c.internal.constantType(Constant(t))
        case _ => tTpe
      }
      q"new TwoFaceInt[$valueTpe]($value)"
    }
  }
}
