import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import internal._

object Test extends App {
  class C[T >: Null] {
    val code = reify{val x: T = "2".asInstanceOf[T]; println("ima worx: %s".format(x)); x}
    println(freeTypes(code.tree))
    val tree = substituteSymbols(code.tree, freeTypes(code.tree), List(definitions.StringClass))
    cm.mkToolBox().eval(tree)
  }

  new C[String]
}