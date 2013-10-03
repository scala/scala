import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test {
  val toolbox = cm.mkToolBox(options = "-Yrangepos")

  def main(args: Array[String]) {
    def test(expr: String) {
      val t = toolbox.parse(expr)
      println(expr)
      println(show(t, printPositions = true))
      println()
    }
    val tests = """
    val x = 0
    var x = 0
    val x, y = 0
    var x, y = 0
    val (x, y) = 0
    """
    val exprs = tests.split("\\n").map(_.trim).filterNot(_.isEmpty)
    exprs foreach test
  }
}
