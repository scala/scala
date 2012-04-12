import scala.reflect.mirror._

final class table extends StaticAnnotation
@table class A

object Test extends App {
  val s = classToSymbol(classOf[A])
  println(s.annotations)
}
