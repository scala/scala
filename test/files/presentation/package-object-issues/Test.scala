import scala.tools.nsc.interactive.tests.InteractiveTest

object Test extends InteractiveTest {

  override protected def filterOutLines(line: String) = 
    line.contains("inaccessible") || line.contains("retrieved ")

}
