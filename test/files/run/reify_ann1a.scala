import scala.reflect._
import scala.reflect.api._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

class ann(bar: List[String]) extends StaticAnnotation

object Test extends App {
  // test 1: reify
  val tree = scala.reflect.Code.lift{
    @ann(bar=List("1a")) @ann(bar=List("1b")) class C[@ann(bar=List("2a")) @ann(bar=List("2b")) T](@ann(bar=List("3a")) @ann(bar=List("3b")) x: T @ann(bar=List("4a")) @ann(bar=List("4b"))) {
      @ann(bar=List("5a")) @ann(bar=List("5b")) def f(x: Int @ann(bar=List("6a")) @ann(bar=List("6b"))) = {
        @ann(bar=List("7a")) @ann(bar=List("7b")) val r = (x + 3): @ann(bar=List("8a")) @ann(bar=List("8b"))
        val s = 4: Int @ann(bar=List("9a")) @ann(bar=List("9b"))
        r + s
      }
    }
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