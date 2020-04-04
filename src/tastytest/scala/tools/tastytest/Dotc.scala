package scala.tools.tastytest

import scala.util.{ Try, Success }

import java.lang.reflect.Modifier

object Dotc {

  private[this] lazy val dotcProcess = processMethod("dotty.tools.dotc.Main")

  def processMethod(mainClassName: String): Array[String] => Try[Boolean] = {
    // TODO call it directly when we are bootstrapped
    val mainClass = Class.forName(mainClassName)
    val reporterClass = Class.forName("dotty.tools.dotc.reporting.Reporter")
    val Main_process = mainClass.getMethod("process", classOf[Array[String]])
    assert(Modifier.isStatic(Main_process.getModifiers), s"$mainClassName.process is not static!")
    val Reporter_hasErrors = reporterClass.getMethod("hasErrors")
    args => Try {
      val reporter  = Main_process.invoke(null, args)
      val hasErrors = Reporter_hasErrors.invoke(reporter).asInstanceOf[Boolean]
      !hasErrors
    }
  }

  def dotc(out: String, classpath: String, sources: String*): Try[Boolean] = {
    if (sources.isEmpty) {
      Success(true)
    }
    else {
      val args = Array(
        "-d", out,
        "-classpath", classpath,
        "-deprecation",
        "-Yerased-terms",
        "-Xfatal-warnings",
        "-usejavacp"
      ) ++ sources
      dotcProcess(args)
    }
  }

  def main(args: Array[String]): Unit = {
    val Array(out, src) = args
    val success = dotc(out, out, src).get
    sys.exit(if (success) 0 else 1)
  }
}
