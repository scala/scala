import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |:type [List(1, 2, 3)]
    |:type List(1, 2, 3)
  """.stripMargin
}

