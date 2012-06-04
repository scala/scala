import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import language.experimental.macros
    |import scala.reflect.runtime.universe._
    |
    |reify
  """.stripMargin
}
