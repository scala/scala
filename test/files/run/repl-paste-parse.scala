
import java.io.{ BufferedReader, StringReader, StringWriter, PrintWriter }

import scala.tools.partest.DirectTest
import scala.tools.nsc.interpreter.ILoop
import scala.tools.nsc.GenericRunnerSettings

object Test extends DirectTest {
  override def extraSettings = s"-usejavacp -i $scriptPath"
  def scriptPath = testPath.changeExtension("script")
  override def newSettings(args: List[String]) = {
    val ss = new GenericRunnerSettings(Console.println)
    ss.processArguments(args, true)
    ss
  }
  def code = ""
  def show() = {
    val r = new BufferedReader(new StringReader(""))
    val w = new StringWriter
    val p = new PrintWriter(w, true)
    new ILoop(r, p).process(settings)
    w.toString.lines foreach { s =>
      if (!s.startsWith("Welcome to Scala")) println(s)
    }
  }
}

