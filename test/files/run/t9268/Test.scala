import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }

  def client1 = """
    class Client1 { def p(p: Partial) = p.toString }
  """

  def client2 = """
    class Client2 { def p(p: Partial) = p.waitFor() }
  """

  def deleteClass(s: String) = {
    val f = new File(testOutput.path, s + ".class")
    assert(f.exists)
    f.delete()
  }

  def show(): Unit = {
    deleteClass("Waiter")
    deleteClass("Waiter$Predicate")

    // Used to crash in Java Generic Signature parsing
    println("Compiling Client1")
    compileCode(client1)
    println(storeReporter.infos.mkString("\n"))
    storeReporter.reset()
    println("Compiling Client2")
    compileCode(client2)
    println(storeReporter.infos.mkString("\n"))
  }
}

