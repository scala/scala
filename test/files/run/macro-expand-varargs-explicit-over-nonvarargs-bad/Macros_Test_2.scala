object Macros {
  def foo(xs: Int*) = macro Impls.foo
  def bar(xs: _*) = macro Impls.foo
}

object Test extends App {
  def test(meth: String): Unit = {
    import scala.reflect.runtime.universe._
    import scala.reflect.runtime.{currentMirror => cm}
    import scala.tools.reflect.ToolBox
    val tree = Apply(Select(Ident(TermName("Macros")), TermName(meth)), List(Typed(Apply(Ident(definitions.ListModule), List(Literal(Constant(1)), Literal(Constant(2)))), Ident(tpnme.WILDCARD_STAR))))
    try cm.mkToolBox().eval(tree)
    catch { case ex: Throwable =>  println(ex.getMessage) }
  }

  test("foo")
  test("bar")
}