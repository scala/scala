import scala.reflect.mirror._

object Test extends App {
  {
    class A
    val c = classToType(classOf[A])
    println(c)
    println(c.typeSymbol == classToSymbol(classOf[A]))
  }
}
