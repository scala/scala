import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
  import scala.reflect.macros.Context
import language.experimental.macros

def impl1(c: Context) = c.universe.Ident(c.universe.TypeName("Int"))
type Foo1 = macro impl1
val x1: Foo1 = 1

def impl2(c: Context)() = c.universe.Ident(c.universe.TypeName("Int"))
type Foo2() = macro impl2
val x2: Foo2() = 2

def impl3(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("Int"))
type Foo3(x: Int)(y: Int) = macro impl3
val x3: Foo3(2)(3) = 3
  """
}