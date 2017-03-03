import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code =
"""def f(x: => Int): Int = x
f _
"""

  // replace indylambda function names by <function1>
  override def eval() = {
    val lines = super.eval
    val r = """\$\$Lambda.*""".r
    lines.map(l => r.replaceAllIn(l, "<function1>"))
  }
}
