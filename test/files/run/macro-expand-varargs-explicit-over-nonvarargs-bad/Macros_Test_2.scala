object Macros {
  def foo(xs: Int*) = macro Impls.foo
}

object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val tree = Apply(Select(Ident(newTermName("Macros")), newTermName("foo")), List(Typed(Apply(Ident(definitions.ListModule), List(Literal(Constant(1)), Literal(Constant(2)))), Ident(tpnme.WILDCARD_STAR))))
  try cm.mkToolBox().eval(tree)
  catch { case ex: Throwable =>  println(ex.getMessage) }
}