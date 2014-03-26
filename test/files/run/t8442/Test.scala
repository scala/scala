import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }

  def app = """
    class C_2 {
      def foo(b: B_1) {
        b.get()
      }
    }
  """

  def show(): Unit = {
    val tClass = new File(testOutput.path, "A_1.class")
    assert(tClass.exists)
    assert(tClass.delete())

    // Expecting stub symbol warning, but no stack trace!
    compileCode(app)
    println(filteredInfos.mkString("\n"))
  }
}
