import scala.reflect.mirror._

class A

object Test extends App {
  val c = classToType(classOf[A])
  println(c)
  println(c.typeSymbol == classToSymbol(classOf[A]))
}
