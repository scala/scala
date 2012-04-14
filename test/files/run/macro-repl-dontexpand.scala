import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-language:experimental.macros"
  def code = """
    |def bar(c: scala.reflect.makro.Context) = ???
    |def foo = macro bar
    |""".stripMargin
}
