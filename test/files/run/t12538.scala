
import scala.tools.partest.DirectTest

object Test extends DirectTest {
  def code =
    """
      |class C {
      |  @java.lang.Deprecated(since = s"test") var i = 4
      |}
    """.stripMargin

  def show() = testUnderJavaAtLeast("17")(assert(compile()))
}
