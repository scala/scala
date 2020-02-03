import scala.tools.partest._

object Test extends ReplTest with Lambdaless {
  def code =
"""def f(x: => Int): Int = x
f _
"""
}
