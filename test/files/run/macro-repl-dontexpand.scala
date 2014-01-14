import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-language:experimental.macros"
  def code = """
    |def bar1(c: scala.reflect.macros.blackbox.Context) = ???
    |def foo1 = macro bar1
    |def bar2(c: scala.reflect.macros.whitebox.Context) = ???
    |def foo2 = macro bar2
    |""".stripMargin
}
