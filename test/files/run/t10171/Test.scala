import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }

  def library = """
package a {
  package b {
    class C { class D }
  }
}
package z {
  class Base {
    type S = String
    def foo(s: S): a.b.C#D = null
  }
  class Sub extends Base {
    def sub = "sub"
  }
}
  """

  def client = """
    class Client { new z.Sub().sub }
  """

  def deleteClass(s: String) = {
    val f = new File(testOutput.path, s + ".class")
    assert(f.exists)
    f.delete()
  }

  def deletePackage(s: String) = {
    val f = new File(testOutput.path, s)
    assert(f.exists)
    f.delete()
  }

  def assertNoErrors(): Unit = {
    assert(storeReporter.infos.isEmpty, storeReporter.infos.mkString("\n"))
    storeReporter.reset()
  }
  def show(): Unit = {
    compileCode(library)
    assertNoErrors()
    deleteClass("a/b/C$D")
    deleteClass("a/b/C")
    deletePackage("a/b")
    compileCode(client)
    assertNoErrors()
  }
}

