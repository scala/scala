
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |class C extends Dynamic
    |class C extends Dynamic
    |class C extends Dynamic
  """.stripMargin
}
