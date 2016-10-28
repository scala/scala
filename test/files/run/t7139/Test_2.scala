import scala.tools.partest.ReplTest

object Test extends ReplTest {
	def code =
  """import test._
    |A(0)
    |A(0)
  """.stripMargin
}
