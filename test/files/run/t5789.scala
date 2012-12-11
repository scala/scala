
import scala.tools.nsc._
import interpreter.ILoop
import scala.tools.partest.ReplTest


object Test extends ReplTest {
  override def extraSettings = "-Yinline"
  def code = """
    val n = 2
    () => n
  """
}

