import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  class C[T >: Null] {
    val code = reify{
      val tt = implicitly[TypeTag[T]]
      println("mah typetag is: %s".format(tt))
    }.tree
    println(code.freeTypes)
    val T = code.freeTypes(0)
    cm.mkToolBox().eval(code, Map(T -> definitions.StringClass.asType))
  }

  new C[String]
}