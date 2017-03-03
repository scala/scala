import scala.tools.partest.ReplTest

object Test extends ReplTest {

  def code = """
import bug._
import Wrap._
object Bar extends Foo
Bar.foo
  """

}
