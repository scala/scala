import scala.tools.partest._

object Test extends StoreReporterDirectTest {
  private def compileCode(): Boolean = {
    new java.io.File("util") mkdirs
    val classpath = List(sys.props("partest.lib"), ".") mkString sys.props("path.separator")
    log(s"classpath = $classpath")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(packageCode)
  }
  def code = ???
  def packageCode = """
package scala.bippy
class A { new util.Random() }
"""
  def show(): Unit = {
    assert(compileCode(), filteredInfos take 1 mkString "")
  }
}
