import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
class Bar(val c: Context) { def impl = { import c.universe._; c.Expr[Unit](q"()") } };def bar: Unit = macro Bar.impl
bar
class Foo(val c: Context) { def impl = { import c.universe._; c.Expr[Unit](q"()") } }
def foo: Unit = macro Foo.impl
foo
  """
}
