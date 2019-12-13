import scala.tools.partest._

object Test extends ReplTest with Lambdaless {
  def code = """
object x { def x = () }
import x._
x
x
  """
}
