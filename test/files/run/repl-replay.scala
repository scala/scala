
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
    |locally { val x = 42 ; "$x" }
    |:replay -Xlint
  """.stripMargin.trim
}
