import scala.tools.nsc.interactive.tests.InteractiveTest
import scala.tools.nsc.util

object Test extends InteractiveTest {
  
  override protected def filterOutLines(line: String) = 
    line.contains("inaccessible") || line.contains("retrieved ")

}
