
object Test extends App {
  import scala.reflect.internal.util.StringContextStripMarginOps
  import scala.reflect.runtime._, universe._
  import scala.tools.reflect.ToolBox

  val mirror = universe.runtimeMirror(universe.getClass.getClassLoader)
  val toolbox = mirror.mkToolBox(options = "-Yrangepos:false")
  def showParsed(code: String) = {
    val parsed = toolbox.parse(code)
    println {
      sm"""|
           |$code
           |${show(parsed, printPositions = true)}"""
    }
  }
  showParsed("2 + 2")
  showParsed("List(42).map(_ + 27)")
}
