object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val tree = Typed(Apply(Select(Ident(newTermName("Macros")), newTermName("foo")), List(Literal(Constant(42)))), Ident(newTypeName("String")))
  try cm.mkToolBox().eval(tree)
  catch { case ex: Throwable =>  println(ex.getMessage) }
}
