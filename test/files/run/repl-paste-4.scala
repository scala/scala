
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = s":paste $pastie\nFoo(new Foo)\n"
  def pastie = testPath.changeExtension("pastie")
}
