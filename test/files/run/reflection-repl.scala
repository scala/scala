import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |scala.reflect.runtime.universe.typeOf[List[Nothing]]
    |""".stripMargin
}
