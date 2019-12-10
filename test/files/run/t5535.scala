import scala.tools.partest._

object Test extends ReplTest with Lambdaless {
  def code = """
def h()(i: Int) = 1 + i
println(h()(5))
val f = h() _
println(f(10))
  """
}
