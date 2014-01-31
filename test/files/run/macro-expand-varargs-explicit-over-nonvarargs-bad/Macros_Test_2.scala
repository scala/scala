object Macros {
  def foo(xs: Int*): Unit = macro Impls.foo
}

object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val tree = Apply(Select(Ident(TermName("Macros")), TermName("foo")), List(Typed(Apply(Ident(definitions.ListModule), List(Literal(Constant(1)), Literal(Constant(2)))), Ident(typeNames.WILDCARD_STAR))))
  try cm.mkToolBox().eval(tree)
  catch { case ex: Throwable =>  println(ex.getMessage) }
}