import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
import scala.language.experimental.macros
import scala.reflect.macros.Macro
trait Bar extends Macro { def impl = c.literalUnit };def bar = macro Bar.impl
bar
trait Foo extends Macro { def impl = c.literalUnit }
def foo = macro Foo.impl
foo
  """
}