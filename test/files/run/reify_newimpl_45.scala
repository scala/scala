import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  class C[T >: Null] {
    val code = reify{val x: T = "2".asInstanceOf[T]; println("ima worx: %s".format(x)); x}
    println(code.tree.freeTypes)
    val T = code.tree.freeTypes(0)
    val tree = code.tree.substituteSymbols(List(T), List(definitions.StringClass))
    cm.mkToolBox().eval(tree)
  }

  new C[String]
}