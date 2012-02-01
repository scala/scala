import scala.reflect.mirror._

object Test extends App {
  class A1

  val c1 = classToType(classOf[A1])
  println(c1)
  println(c1.typeSymbol == classToSymbol(classOf[A1]))

  new Test
}

class Test {
  class A2

  val c2 = classToType(classOf[A2])
  println(c2)
  println(c2.typeSymbol == classToSymbol(classOf[A2]))
}
