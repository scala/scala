
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
import scala.reflect.io.File
:imports
  """

  // 4) import scala.reflect.io.File   (1 types, 1 terms)
  lazy val stats = """\(.*\)""".r

  override def normalize(s: String) = stats.replaceFirstIn(s, "(...)")
}
