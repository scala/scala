import scala.tools.partest._

object Test extends DirectTest {
  def code = ???

  def macros_1 = """
    import scala.reflect.macros.blackbox.Context

    object Impls {
      def impl(c: Context) = { import c.universe._; c.Expr[Unit](q"()") }
    }

    object Macros {
      //import Impls._
      def impl(c: Context) = { import c.universe._; c.Expr[Unit](q"()") }
      def foo: Unit = macro impl
    }
  """
  def compileMacros() = {
    val classpath = pathOf(sys.props("partest.lib"), sys.props("partest.reflect"))
    compileString(newCompiler("-language:experimental.macros", "-cp", classpath))(macros_1)
  }

  def test_2 = """
    object Test extends App {
      println(Macros.foo)
    }
  """
  def compileTest() = {
    val classpath = pathOf(sys.props("partest.lib"), testOutput.path)
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(test_2)
  }

  def show(): Unit = {
    log("Compiling Macros_1...")
    if (compileMacros()) {
      log("Compiling Test_2...")
      if (compileTest()) log("Success!") else log("Failed...")
    }
  }
}
