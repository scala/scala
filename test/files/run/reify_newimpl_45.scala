import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  class C[T >: Null] {
    val code = reify{val x: T = "2".asInstanceOf[T]; println("ima worx: %s".format(x)); x}
    println(code.tree.freeTypes)
    val T = code.tree.freeTypes(0)
    cm.mkToolBox().runExpr(code.tree, Map(T -> definitions.StringClass.asType))
  }

  new C[String]
}