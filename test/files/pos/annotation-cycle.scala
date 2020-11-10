import scala.annotation.StaticAnnotation

class anno(attr: Boolean) extends StaticAnnotation

object Test {
  @anno(attr = true) def attr: Int = 1
}
