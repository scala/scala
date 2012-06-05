object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val tree = Select(Ident("Macros"), newTermName("foo"))
  try cm.mkToolBox().runExpr(tree)
  catch { case ex: Throwable =>  println(ex.getMessage) }
}
