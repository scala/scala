import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xlog-free-terms"
  def code = """
import scala.reflect.mirror._
{
  val x = 2
  val code = reify {
    x
  }
  println(code.eval)
}
  """
}
