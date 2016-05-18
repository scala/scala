
import scala.tools.partest.ReplTest

object MyApp extends App {
  Console println "Hello, delayed world."
}

object Test extends ReplTest {
  def code = ":javap -app MyApp$"

  override def show() = {
    val coded  = "Code:"
    val strung = "String Hello, delayed world."
    val lines  = eval().toList
    assert (lines.count(s => s.endsWith(coded)) == 1)
    assert (lines.count(s => s.endsWith(strung)) == 1)
  }
}
