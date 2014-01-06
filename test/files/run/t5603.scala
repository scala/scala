import scala.tools.partest._
import java.io._
import scala.tools.nsc._
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, Settings, CompilerCommand}
import scala.tools.nsc.reporters.ConsoleReporter

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:parser -Ystop-after:parser -d " + testOutput.path

  override def code = """
    trait Greeting {
      val name: String
      val msg = "How are you, "+name
    }
    class C(i: Int) extends {
      val nameElse = "Bob"
    } with Greeting {
      val name = "avc"
      println(msg)
    }

    object Test extends App {}
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
    settings.Yrangepos.value = true
    val command = new CompilerCommand((CommandLineParser tokenize extraSettings) ++ args.toList, settings)
    Global(command.settings, new ConsoleReporter(settings))
  }
}
