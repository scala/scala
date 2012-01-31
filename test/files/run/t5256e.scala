import scala.reflect.mirror._

class C { class A }

object Test extends App {
  val c = classToType(classOf[C#A])
  println(c)
  println(c.typeSymbol == classToSymbol(classOf[C#A]))
}
