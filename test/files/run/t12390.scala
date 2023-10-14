import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter._, shell._
import scala.util.Properties.lineSeparator

import java.io.{PrintWriter, StringWriter}

object Test {

  def main(args: Array[String]): Unit = {
    val code: String =
      s"""|import scala.io.Source
          |import scala.util.Properties.lineSeparator
          |import scala.util.chaining._
          |Source.fromFile(sys.props("partest.test-path")).pipe(s => s.getLines().mkString(lineSeparator).tap(_ => s.close()))
          |""".stripMargin.linesIterator.mkString(lineSeparator)

    val s = new Settings()

    s.processArguments(
      List(
        "-deprecation",
        "-Yrepl-class-based",
        "-Yrepl-outdir", "target"
      ), processAll = true)

    val drain = new StringWriter //new OutputStream { override def write(byte: Int) = () }
    val sinkWriter = new PrintWriter(drain)
    val reporter = new ReplReporterImpl(ShellConfig(s), s, sinkWriter)
    val repl = new IMain(s, reporter)
    repl.settings.usejavacp.value = true
    for (i <- 1 to 65) {
      repl.interpret(code) match {
        case Results.Success =>
          assert(repl.valueOfTerm(repl.mostRecentVar).get != null)  // 2.12: null after 60
        case other =>
          println(drain.toString)
          throw new MatchError(other)
      }
    }
  }
}
/*
   JavaMirror.scalaSimpleName throws on the long class name and valueOfTerm ignores the error.

   scalaSimpleName expects the wrapper class name to have its enclosing class name as a prefix,
   but some special encoding kicks in at 64 chars
*/
