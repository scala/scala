import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import Macros._

object Test extends App {
  val tb = cm.mkToolBox()
  try tb.compile(Select(Ident(TermName("Macros")), TermName("foo")))
  catch { case ToolBoxError(message, _) => println("""macroSynthetic-.*?\.scala""".r.replaceAllIn(message, "<synthetic file name>")) }
}