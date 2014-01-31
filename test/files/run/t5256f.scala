import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  class A1 { def foo = ??? }

  val c1 = cm.classSymbol(classOf[A1])
  println(c1)
  println(c1.fullName)
  println(c1.info)

  new Test
}

class Test {
  class A2 { def foo = ??? }

  val c2 = cm.classSymbol(classOf[A2])
  println(c2)
  println(c2.fullName)
  println(c2.info)
}
