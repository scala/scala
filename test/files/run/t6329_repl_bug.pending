import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import scala.reflect.runtime.universe._
    |import scala.reflect.runtime._
    |classManifest[List[_]]
    |scala.reflect.classTag[List[_]]
    |""".stripMargin
}
