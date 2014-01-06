object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val tree = Apply(Select(Ident(TermName("Macros")), TermName("foo")), List(Literal(Constant(42))))
  println(cm.mkToolBox().eval(tree))
}
