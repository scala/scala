
import scala.tools.partest.ScriptTest

object Test extends ScriptTest {
  // must be called Main to get probing treatment in parser
  override def testmain = "Main"
}
