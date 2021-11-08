import scala.annotation.StaticAnnotation
import scala.reflect.internal.annotations.uncheckedBounds

object Test {
  trait Chars[A <: CharSequence]
  trait Two[A, B]
  class ann[A] extends StaticAnnotation
  @ann[Chars[Int]] val x = 42
  val y: Two[Chars[Long] @uncheckedBounds, Chars[Double]] = null
}
