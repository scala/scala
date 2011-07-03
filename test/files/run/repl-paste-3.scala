import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
println(3)
  List(1,2)
  """
}