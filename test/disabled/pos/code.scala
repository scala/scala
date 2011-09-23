import reflect.runtime.Mirror.ToolBox
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings

object Test extends App {
  def foo(ys: List[Int]) = {
    val fun: reflect.Code[Int => Int] = x => x + ys.length
    fun
  }
  val code = foo(List(2))
  val tree = code.tree.asInstanceOf[scala.reflect.runtime.Mirror.Tree]
  val targetType = code.manifest.tpe.asInstanceOf[scala.reflect.runtime.Mirror.Type]
  println("testing: "+tree)
  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter, args mkString " ")
  val ttree = toolbox.typeCheck(tree, targetType)
  println("result = "+ttree)
}


