import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
import scala.language.dynamics
class Dyn(m: Map[String, Any]) extends Dynamic { def selectDynamic[T](s: String): T = m(s).asInstanceOf[T] }
new Dyn(Map("foo" -> 10)).foo[Int]
  """
}
