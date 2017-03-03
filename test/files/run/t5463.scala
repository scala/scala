import scala.reflect.internal.FatalError
import scala.tools.partest.DirectTest

object Test extends DirectTest {

  def code = "class A"

  override def show(): Unit = {
    // Create a broken JAR file and put it on compiler classpath
    val jarpath = testOutput.path + "/notajar.jar"
    scala.reflect.io.File(jarpath).writeAll("This isn't really a JAR file")

    val classpath = List(sys.props("partest.lib"), jarpath, testOutput.path) mkString sys.props("path.separator")
    try {
      compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
      throw new Error("Compilation should have failed");
    } catch {
      case ex: FatalError => // this is expected
    }
  }
}
