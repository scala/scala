object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val tree = Apply(Select(Ident("Macros"), newTermName("foo")), List(Literal(Constant(42))))
  println(cm.mkToolBox().runExpr(tree))
}
