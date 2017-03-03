import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
def h()(i: Int) = 1 + i
println(h()(5))
val f = h() _
println(f(10))
  """

  // replace indylambda function names by <function1>
  override def eval() = {
    val lines = super.eval
    val r = """\$\$Lambda.*""".r
    lines.map(l => r.replaceAllIn(l, "<function1>"))
  }
}
