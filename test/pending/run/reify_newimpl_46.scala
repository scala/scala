import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  class C[T[_] >: Null] {
    val code = reify{val x: T[String] = null; println("ima worx"); x}.tree
    println(code.freeTypes)
    val T = code.freeTypes(0)
    cm.mkToolBox().eval(code, Map(T -> definitions.ListClass.asType))
  }

  new C[List]
}