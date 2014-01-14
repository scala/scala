import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import language.experimental.macros
    |import scala.reflect.macros.blackbox.Context
    |
    |object Impls {
    |  def foo(c: Context)(x: c.Expr[Int]) = {
    |    import c.universe._
    |    val body = Apply(Select(x.tree, TermName("$plus")), List(Literal(Constant(1))))
    |    c.Expr[Int](body)
    |  }
    |
    |  def bar(c: Context)(x: c.Expr[Int]) = {
    |    import c.universe._
    |    val body = Apply(Select(x.tree, TermName("$plus")), List(Literal(Constant(2))))
    |    c.Expr[Int](body)
    |  }
    |
    |  def quux(c: Context)(x: c.Expr[Int]) = {
    |    import c.universe._
    |    val body = Apply(Select(x.tree, TermName("$plus")), List(Literal(Constant(3))))
    |    c.Expr[Int](body)
    |  }
    |}
    |object Macros {
    |  object Shmacros {
    |    def foo(x: Int): Int = macro Impls.foo
    |  }
    |  def bar(x: Int): Int = macro Impls.bar
    |}; class Macros {
    |  def quux(x: Int): Int = macro Impls.quux
    |}
    |
    |import Macros.Shmacros._
    |println(foo(2) + Macros.bar(2) * new Macros().quux(4))
    |""".stripMargin
}
