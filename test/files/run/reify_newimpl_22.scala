import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xlog-free-terms"
  def code = """
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval
{
  val x = 2
  val code = reify {
    x
  }
  println(code.eval)
}
  """
}