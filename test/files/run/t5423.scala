import java.lang.Class
import scala.reflect.mirror._
import scala.reflect.runtime.Mirror.ToolBox
import scala.reflect.Code

final class table extends StaticAnnotation
@table class A

object Test extends App {
  val s = classToSymbol(classOf[A])
  println(s.annotations)
}
