import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Wconf:any:error"

  def code = """
42
  """.trim
}
