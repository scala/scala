import scala.tools.partest.{ReplTest, Hashless}

object Test extends ReplTest with Hashless {
  def code = """
object x { def x = () }
import x._
x
x
  """
}
