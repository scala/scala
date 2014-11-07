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
  """

  def library2 = """
    package pack2
    trait U extends pack1.T
  """

  def app = """
    package pack3
    object X {
      trait U
    }
    import X._
    import pack2._

    trait V extends U
  """

  def show(): Unit = {
    Seq(library1, library2) foreach compileCode
    assert(filteredInfos.isEmpty, filteredInfos)

    // blow away the entire package
    val pack1 = new File(testOutput.path, "pack1")
    val tClass = new File(pack1, "T.class")
    assert(tClass.exists)
    assert(tClass.delete())
    assert(pack1.delete())

    // should report ambiguous import, despite the fact that a parent of pack2.U is absent
    compileCode(app)
    println(filteredInfos.mkString("\n"))
  }
}
