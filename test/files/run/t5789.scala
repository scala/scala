
import scala.tools.nsc._
import interpreter.ILoop
import scala.tools.partest.ReplTest


object Test extends ReplTest {
  override def extraSettings = "-opt:l:classpath"
  def code = """
    val n = 2
    () => n
  """

  // replace indylambda function names by <function0>
  override def eval() = {
    val lines = super.eval
    val r = """\$\$Lambda.*""".r
    lines.map(l => r.replaceAllIn(l, "<function0>"))
  }
}

