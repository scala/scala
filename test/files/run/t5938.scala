import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String =
    s"-usejavacp -d ${testOutput.path}"

  override def code = """
object O extends C {
  def main(args: Array[String]): Unit = {
  }
  // Static forwarder for foo and setter_foo_= added more once in a multi-run compile.
}
  """.trim

  override def show(): Unit = {
    val global = newCompiler()
    Console.withErr(System.out) {
      compileString(global)(code)
      compileString(global)(code)
      loadClass // was "duplicate name and signature in class X"
    }
  }

  def loadClass: Class[_] = {
    val cl = new java.net.URLClassLoader(Array(testOutput.toFile.toURL));
    cl.loadClass("O")
  }
}

trait T {
  val foo: String = ""
}
class C extends T

