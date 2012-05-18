import scala.annotation.StaticAnnotation

class ann(val bar: Any) extends StaticAnnotation

object Api {
  @ann({def foo = "foo!!"})
  def foo = println("foo")
}
