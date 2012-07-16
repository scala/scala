import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |case class X(s: String)
    |scala.reflect.runtime.universe.typeOf[X]
    |""".stripMargin
}
