import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

object Test extends App {
  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val tree = tb.parse("""
    import scala.reflect.runtime.{universe => ru}
    ru
  """)
  val ttree = tb.typecheck(tree)

  def stabilizeIds(s: String) = """#\d+""".r.replaceAllIn(s, "#<id>")
  def stabilizePositions(s: String) = """\d+""".r.replaceAllIn(s, "<offset>")
  def stabilize(s: String) = stabilizePositions(stabilizeIds(s))
  println(stabilize(showRaw(ttree)))
  println(stabilize(showRaw(ttree, printIds = true)))
}