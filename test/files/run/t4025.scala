import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
class Color(val red: Int)

case class Red(r:Int) extends Color(r)

def f(c: Any) = c match { case Red(_) => () }
"""
}
