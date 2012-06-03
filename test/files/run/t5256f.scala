import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  class A1 { def foo = ??? }

  val c1 = cm.reflectClass(classOf[A1]).symbol
  println(c1)
  println(c1.fullName)
  println(c1.typeSignature)

  new Test
}

class Test {
  class A2 { def foo = ??? }

  val c2 = cm.reflectClass(classOf[A2]).symbol
  println(c2)
  println(c2.fullName)
  println(c2.typeSignature)
}
