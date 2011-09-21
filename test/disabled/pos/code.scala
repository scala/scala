import reflect.runtime.Mirror.ToolBox
import scala.tools.nsc.reporters._

object Test extends App {
  def foo[T](ys: List[T]) = {
    val fun: reflect.Code[Int => Int] = x => x + ys.length
    fun
  }
  val code = foo(List(2))
  val tree = foo(List(2)code.tree.asInstanceOf[scala.reflect.runtime.Mirror.Tree]
  val targetType = code.manifest.tpe
  val reporter = new StoreReporter
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(tree, targetType)
  if (reporter.infos.nonEmpty) {
    reporter.infos foreach println
    println("compilaton failed")
  } else {
    println("result = "+ttree)
  }
}

