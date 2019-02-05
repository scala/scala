package m

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait HList
trait Coproduct

abstract class Encoder[T]

object Macros {
  // these two implicits may be deemed ambiguous if the type argument is not checked for bounds
  implicit def encodeHList[R <: HList]: Encoder[R] = macro DerivationMacros.encodeHList[R]
  implicit def encodeCoproduct[R <: Coproduct]: Encoder[R] = macro DerivationMacros.encodeCoproduct[R]
}


object Auto {
  final def deriveEncoder[A](implicit encode: Encoder[A]): Encoder[A] =
    encode
}

class DerivationMacros(val c: whitebox.Context) {
  import c.universe._

  def encodeHList[R <: HList](implicit R: c.WeakTypeTag[R]): c.Expr[Encoder[R]] = {
    c.Expr[Encoder[R]](
      q"""
        {
          def e(a: $R): Object = a
          Predef.???
        }
        """
    )
  }

  def encodeCoproduct[R <: Coproduct](implicit R: c.WeakTypeTag[R]): c.Expr[Encoder[R]] = {
    c.Expr[Encoder[R]](
      q"""
        {
          def e(a: $R): Object = a

          Predef.???
        }
        """
    )
  }
}
