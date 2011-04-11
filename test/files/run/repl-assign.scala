import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
var x = 10
var y = 11
x = 12
y = 13
  """
}