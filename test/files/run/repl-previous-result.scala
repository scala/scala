import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
    |"foobar"
    |.size
  """.stripMargin.trim
}
