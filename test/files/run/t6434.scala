import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code =
"""def f(x: => Int): Int = x
f _
"""
}
