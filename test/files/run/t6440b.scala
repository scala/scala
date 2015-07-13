import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }

  def library1 = """
    package pack1
    trait T
    class U {
      def t = new T {}
      def one = 1
    }
  """

  def library2 = """
    package pack2
    object V {
      def u = new pack1.U
    }
    """

  def app1 = """
    package pack3
    object Test {
      pack2.V.u.one // okay
    }
  """

  def app2 = """
    package pack3
    object Test {
      pack2.V.u.t // we have to fail if T.class is missing
    }
  """

  def show(): Unit = {
    compileCode(library1)
    val pack1 = new File(testOutput.path, "pack1")
    val tClass = new File(pack1, "T.class")
    assert(tClass.exists)
    assert(tClass.delete())

    // allowed to compile, no direct reference to `T`
    compileCode(library2)
    assert(filteredInfos.isEmpty, filteredInfos)

    // allowed to compile, no direct reference to `T`
    compileCode(app1)
    assert(filteredInfos.isEmpty, filteredInfos)

    // bad symbolic reference error expected (but no stack trace!)
    compileCode(app2)
    println(filteredInfos.mkString("\n"))
  }
}
