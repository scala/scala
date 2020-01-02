
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = s":load $script"
  def script = testPath.changeExtension("script")
}

// was: <script>:1: error: expected class or object definition
// now: error: '}' expected but eof found.

