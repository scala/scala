import scala.annotation.StaticAnnotation

class ann(val bar: Any) extends StaticAnnotation

object Api {
  @ann({def baz = "baz!!"})
  def foo = println("foo")
}
