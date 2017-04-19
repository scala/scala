import scala.tools.partest.ReplTest

// Fails on 2.11, fixed on 2.12
object Test extends ReplTest {

  def code = """
import bug._
import Wrap._
object Bar extends Foo
Bar.foo
  """

}
