import scala.tools.partest._

object Test extends DirectTest {
  def code = ???

  def macros_1 = """
    import scala.reflect.macros.Context

    object Impls {
      def impl(c: Context) = c.literalUnit
    }

    object Macros {
      //import Impls._
      def impl(c: Context) = c.literalUnit
      def foo = macro impl
    }
  """
  def compileMacros() = {
    val classpath = List(sys.props("partest.lib"), sys.props("partest.reflect")) mkString sys.props("path.separator")
    compileString(newCompiler("-language:experimental.macros", "-cp", classpath, "-d", testOutput.path))(macros_1)
  }

  def test_2 = """
    object Test extends App {
      println(Macros.foo)
    }
  """
  def compileTest() = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
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