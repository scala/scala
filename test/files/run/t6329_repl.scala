import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |classManifest[List[_]]
    |scala.reflect.classTag[List[_]]
    |""".stripMargin
}
