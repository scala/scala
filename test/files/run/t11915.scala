
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
  |:power
  |val x = 42
  |val y = intp.valueOfTerm("x")
  |:reset
  |val x = 27
  |val y = intp.valueOfTerm("x")
  |:quit""".stripMargin

  def valsOnly(s: String) = {
    val r = raw"val [xy]:".r
    r.findPrefixOf(s).nonEmpty
  }
  override def eval() = super.eval().filter(valsOnly)
}
