import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xmacros"
  def code = """
    |def macro foo = ???
    |""".stripMargin
}