import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
object x { def x={} }
import x._
x
x
  """
}
