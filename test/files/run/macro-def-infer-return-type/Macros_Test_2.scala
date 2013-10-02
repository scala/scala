object Macros1 {
  def foo(x: Int) = macro Impls1.foo
}

object Macros2 {
  def foo[T](x: T) = macro Impls2.foo[T]
}

object Macros3 {
  def foo[T](x: T) = macro Impls3.foo[T]
}

object Test extends App {
  println(Macros1.foo(42))

  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val tree = Apply(Select(Ident(TermName("Macros2")), TermName("foo")), List(Literal(Constant(42))))
  try cm.mkToolBox().eval(tree)
  catch { case ex: Throwable =>  println(ex.getMessage) }

  println(Macros3.foo(42))
}
