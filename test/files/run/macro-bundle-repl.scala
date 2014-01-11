import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Macro
trait Bar extends Macro { def impl = { import c.universe._; c.Expr[Unit](q"()") } };def bar: Unit = macro Bar.impl
bar
trait Foo extends Macro { def impl = { import c.universe._; c.Expr[Unit](q"()") } }
def foo: Unit = macro Foo.impl
foo
  """
}
