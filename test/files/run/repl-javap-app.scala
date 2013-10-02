
import scala.tools.partest.ReplTest

object MyApp extends App {
  Console println "Hello, delayed world."
}

object Test extends ReplTest {
  def code = ":javap -app MyApp$"
}
