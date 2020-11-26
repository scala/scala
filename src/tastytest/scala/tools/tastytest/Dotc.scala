package scala.tools.tastytest

import scala.util.{ Try, Success }

import java.lang.reflect.Modifier

object Dotc extends Script.Command {

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

  def dotc(out: String, classpath: String, additionalSettings: Seq[String], sources: String*): Try[Boolean] = {
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
      ) ++ additionalSettings ++ sources
      dotcProcess(args)
    }
  }

  val commandName: String = "dotc"
  val describe: String = s"$commandName <out: Directory> <src: File>"

  def process(args: String*): Int = {
    if (args.length != 2) {
      println(red(s"please provide two arguments in sub-command: $describe"))
      return 1
    }
    val Seq(out, src) = args: @unchecked
    val success = dotc(out, out, Nil, src).get
    if (success) 0 else 1
  }

}
