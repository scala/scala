import scala.tools.partest.ReplTest
import scala.tools.nsc.Settings

object Test extends ReplTest {
  override def extraSettings = s"-Yrepl-outdir ${testOutput.path}"

  def code = """
case class Bippy(x: Int)
val x = Bippy(1)
$intp.reporter.withoutUnwrapping {
  println($intp.showDirectory)
}
  """

}
