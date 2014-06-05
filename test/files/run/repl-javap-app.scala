
import scala.tools.partest.ReplTest

object MyApp extends App {
  Console println "Hello, delayed world."
}

object Test extends ReplTest {
  def code = ":javap -app MyApp$"

  override def welcoming = true

  // The constant pool indices are not the same for GenASM / GenBCode, so
  // replacing the exact numbers by XX.
  lazy val hasConstantPoolRef = """(.*)(#\d\d)(.*)""".r

  override def normalize(s: String) = s match {
    case hasConstantPoolRef(start, ref, end) => start + "#XX" + end
    case _ => super.normalize(s)
  }
}
