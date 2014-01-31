import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  {
    class A { def foo = ??? }
    val c = cm.classSymbol(classOf[A])
    println(c)
    println(c.fullName)
    println(c.info)
  }
}