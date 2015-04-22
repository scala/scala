import java.io.File

import scala.tools.partest.StoreReporterDirectTest

object Test extends StoreReporterDirectTest {

  def code = ""

  override def show(): Unit = {
    deleteClass("HasInner$Inner")
    println("1. Don't refer to Inner")
    compileCode("class Test { def test(x: HasInner) = x }")
    assert(filteredInfos.isEmpty, filteredInfos)
    println("2. Refering to Inner")
    compileCode("class Test { def test(x: HasInner#Inner) = x }")
    println(filteredInfos.mkString("\n"))
  }

  def deleteClass(name: String) {
    val classFile = new File(testOutput.path, name + ".class")
    assert(classFile.exists)
    assert(classFile.delete())
  }

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }
}
