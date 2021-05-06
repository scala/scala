import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter._, shell._

import java.io.{PrintWriter, OutputStream}

object Test {

  def main(args: Array[String]): Unit = {
    //val filename: String = ".../data/generated_01mb.base64"
    val filename: String = sys.props("partest.test-path")
    val code: String =
        "import scala.io.Source\n " +
        "Source.fromFile(\"" + filename + "\").getLines().mkString(\"\\n\")"

    val s = new Settings()

    s.processArguments(
      List(
        //"-Xprint:typer",
        "-deprecation",
        "-Yrepl-class-based",
        "-Yrepl-outdir", "./target"
      ), processAll = true)

    val drain = new OutputStream { override def write(byte: Int) = () }
    val sinkWriter = new PrintWriter(drain)
    val reporter = new ReplReporterImpl(ShellConfig(s), s, sinkWriter)
    val repl = new IMain(s, reporter)
    repl.settings.usejavacp.value = true
    for(i <- 1 to 65) {
      repl.interpret(code) match {
        case Results.Success =>
          assert(repl.valueOfTerm(repl.mostRecentVar).get != null)  // 2.12: null after 60
        case other =>
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
