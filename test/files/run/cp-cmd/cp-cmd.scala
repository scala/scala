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
  val commands = Seq(
    "if (getClass.getClassLoader.getResourceAsStream(\"test.txt" +
      "\") eq null) \"not found\" else \"found\"",
    ":cp test/files/run/cp-cmd",
    "println(new java.io.BufferedReader(new java.io.InputStreamReader(" +
      "getClass.getClassLoader.getResourceAsStream(\"test.txt\"))).readLine)",
    "getClass.getClassLoader.loadClass(\"ClasspathTest\")",
    "ClasspathTest.test",
    ":q")
  val loop = new TestLoop(commands)

  loop.process(settings)
  println()
}

class TestLoop(
  commands: Seq[String]
) extends ILoop(
  Some(new BufferedReader(new StringReader(commands.mkString("\n")))),
  new JPrintWriter(Console.out, true)
) {
  override protected def echo(msg: String) = ()
}
