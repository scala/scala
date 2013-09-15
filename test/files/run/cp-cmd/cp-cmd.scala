import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop
import scala.tools.nsc.interpreter.JPrintWriter

import java.io.File
import java.io.StringReader
import java.io.BufferedReader

/**
 * Not using ReplTest because the :cp command would print the current classpath.
 */
object Test extends App {
  val settings = new Settings
  settings.usejavacp.value = true

  /**
   * On the first run the tmp file is not found since its folder is not on the classpath.
   * Hence "not found" should be printed.
   * After :cp a replay is triggered. This time "found" should be printed since it now is
   * on the classpath.
   *
   * Just to be sure we then print the content of the file which should be "haggis".
   */
  val code = """
    |if (getClass.getClassLoader.getResourceAsStream("test.txt") eq null) "not found" else "found"
    |:cp test/files/run/cp-cmd
    |{
    |  import java.io._
    |  val in = getClass.getClassLoader.getResourceAsStream("test.txt")
    |  val reader = new BufferedReader(new InputStreamReader(in))
    |  println(reader.readLine)
    |}
    |getClass.getClassLoader.loadClass("ClasspathTest")
    |ClasspathTest.test
    |:q
  """.stripMargin.trim

  val loop = new TestLoop(code)

  loop.process(settings)
  println()
}

class TestLoop(
  code: String
) extends ILoop(
  Some(new BufferedReader(new StringReader(code))),
  new JPrintWriter(Console.out, true)
) {
  override protected def echo(msg: String) = ()
}
