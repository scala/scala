import scala.annotation.StaticAnnotation

class anno(a: Any) extends StaticAnnotation

object Test {
  def foo = attr
  @anno(foo) def attr: Int = foo
}
