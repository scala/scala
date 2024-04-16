//> using options -deprecation -Werror
import scala.annotation.{StaticAnnotation, nowarn}
import scala.reflect.internal.annotations.uncheckedBounds

object Test {
  trait Chars[A <: CharSequence]
  trait Two[A, B]
  class ann[A] extends StaticAnnotation
  @ann[Chars[Int]] val x = 42
  val y: Two[Chars[Long] @uncheckedBounds, Chars[Double]] = null
  def z: Chars[X forSome { type X <: Int }] = null

  @deprecated type DeprecatedAlias = String
  @deprecated class DeprecatedClass
  @nowarn("cat=deprecation") type UndeprecatedAlias = DeprecatedClass

  ("": Any) match {
    case _: DeprecatedAlias =>
    case _: DeprecatedClass =>
    case _: UndeprecatedAlias => // no warning here
  }
}
