import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

class C

object Test extends App {
  val Block(List(ValDef(_, _, tpt: CompoundTypeTree, _)), _) = reify{ val x: C { def x: Int } = ??? }.tree
  println(tpt)
  println(tpt.templ.parents)
  println(tpt.templ.self)
  println(tpt.templ.body)
}
