import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xlog-runtime-splices"
  def code = """
import scala.reflect.mirror._
{
  val x = 2
  val code = reify {
    val y = reify { x }
    y.eval
  }
  println(code.eval)
}
  """
}
