import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
  val v: java.util.ArrayList[String] = new java.util.ArrayList[String](5)
  val v: java.util.ArrayList[String] = new java.util.ArrayList[String](5)
  """
}