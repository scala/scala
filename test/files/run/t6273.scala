import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def tq = "\"\"\""
  def code = s"""
val y = 55
val x = s$tq
  y = $$y
$tq
  """
}
