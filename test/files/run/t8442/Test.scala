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
      def foo(b: B_1): Unit = {
        b.get()
      }
    }
  """

  def show(): Unit = {
    val tClass = new File(testOutput.path, "A_1.class")
    assert(tClass.exists)
    assert(tClass.delete())

    // Expecting stub symbol warning only under -verbose, but definitely no stack trace!
    compileCode(app)
    assert(filteredInfos.isEmpty, filteredInfos.mkString("; "))
  }
}
