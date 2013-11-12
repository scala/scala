import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
import scala.language.experimental.macros
import scala.reflect.macros.BlackboxMacro
trait Bar extends BlackboxMacro { def impl = { import c.universe._; c.Expr[Unit](q"()") } };def bar = macro Bar.impl
bar
trait Foo extends BlackboxMacro { def impl = { import c.universe._; c.Expr[Unit](q"()") } }
def foo = macro Foo.impl
foo
  """
}
