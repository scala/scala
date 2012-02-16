import scala.reflect._
import scala.reflect.api._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

class ann(bar: String, quux: Array[String] = Array(), baz: ann = null) extends ClassfileAnnotation

object Test extends App {
  // test 1: reify
  val tree = scala.reflect.Code.lift{
    @ann(bar="1", quux=Array("2", "3"), baz = new ann(bar = "4")) class C
  }.tree
  println(tree.toString)

  // test 2: import and typecheck
  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(tree)
  println(ttree.toString)

  // test 3: import and compile
  toolbox.runExpr(tree)
}