object Test {
  import scala.tools.nsc._
  import interpreter._
  import java.io.PrintWriter

  class NullOutputStream extends OutputStream { def write(b: Int) { } }

  def main(args: Array[String]) {
    val settings = new Settings
    settings.classpath.value = System.getProperty("java.class.path")

    val intp = new IMain(settings, new PrintWriter(new NullOutputStream))
    intp.interpret("def x0 = 123")
    intp.interpret("val x1 = x0 * 2")
    println(intp.valueOfTerm("x1"))
  }
}
