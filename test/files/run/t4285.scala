import scala.tools.partest.ReplTest
object Test extends ReplTest {
  def code = """
    |val x = Array(1,2,3,4,5,6,7)
    |val y = x transform (_ * 2)
    |println(y.sum)
  """.stripMargin
}
