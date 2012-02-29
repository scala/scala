import scala.tools.partest._
import java.io._
import scala.tools.nsc._
import scala.tools.nsc.util.CommandLineParser
import scala.tools.nsc.doc.{Settings, DocFactory}
import scala.tools.nsc.reporters.ConsoleReporter

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:parser -Yrangepos -Ystop-after:parser -d " + testOutput.path

  override def code = """
    // SI-5527
    trait Test {
      def sth {
        /** Some comment here */
        object Maybe {
          /** Some comment inside */
          def nothing() = ()
        }
      }
    }
  """

  override def show(): Unit = {
    // redirect err to out, for logging
    val prevErr = System.err
    System.setErr(System.out)
    compile()
    System.setErr(prevErr)
  }

  override def newCompiler(args: String*): Global = {
    // we want the Scaladoc compiler here, because it keeps DocDef nodes in the tree
    val settings = new Settings(_ => ()) 
    val command = new ScalaDoc.Command((CommandLineParser tokenize extraSettings) ++ args.toList, settings)
    new DocFactory(new ConsoleReporter(settings), settings).compiler
  }

  override def isDebug = false // so we don't get the newSettings warning
}


