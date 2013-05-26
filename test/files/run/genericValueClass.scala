
import scala.language.implicitConversions

object Test extends App {
  class ArrowAssocClass[A](val __leftOfArrow: A) extends AnyVal {
    @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(__leftOfArrow, y)
    def →[B](y: B): Tuple2[A, B] = ->(y)
  }

  {
  @inline implicit def ArrowAssoc[A](x: A): ArrowAssocClass[A] = new ArrowAssocClass(x)
  val x = 1 -> "abc"
  println(x)
  }

  {
    val y = 2 -> "def"
    println(y)
  }
}
