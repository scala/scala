import scala.tools.partest.ReplTest

object Test extends ReplTest {
  // Filter out the abbreviated stacktrace "... X elided" 
  // because the number seems to differ between versions/platforms/...
  override def show = eval() filterNot (_ contains "elided") foreach println
  def code =
"""
val 1 = 2
val List(1) = List(1)
"""
}
