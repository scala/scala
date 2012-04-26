import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
def h()(i: Int) = 1 + i
println(h()(5))
val f = h() _
println(f(10))
  """
}
