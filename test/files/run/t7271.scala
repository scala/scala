import scala.tools.partest._
import scala.tools.nsc._
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, Settings, CompilerCommand}
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.internal.Positions

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:parser -Ystop-after:parser -d " + testOutput.path

  override def code = """
    class C {
      def quote = s"foo${this}baz"
      def tripleQuote = s"foo${this}baz"
    }
  """.trim

  override def show(): Unit = {
    // redirect err to out, for logging
    val prevErr = System.err
    System.setErr(System.out)
    compile()
    System.setErr(prevErr)
  }

  override def newCompiler(args: String*): Global = {

    val settings = new Settings()
    settings.Xprintpos.value = true
    val command = new CompilerCommand((CommandLineParser tokenize extraSettings) ++ args.toList, settings)
    new Global(command.settings, new ConsoleReporter(settings)) with Positions
  }
}
