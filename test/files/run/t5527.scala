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
    object UselessComments {

      var z = 0

      def test1 = {
        /** Some comment here */
        object Maybe {
          /** Some comment inside */
          def nothing() = ()
        }
      }

      def test2 = {
        var x = 4
        if (true) {
          /** Testing 123 */
          x = 5
          val y = 6
        }
      }

      def test3 = {
        if (true)
         z = 3

        /** Calculate this result. */
        val t = 4
        for (i <- 0 to 4)
          println(i)
      }

      val test4 = ('a') match {
        /** Another digit is a giveaway. */
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  =>
          true
        case _ =>
          false
      }
    }

    /** comments that we should keep */
    object UsefulComments {
      /** class A */
      class A {
        /** f */
        def f(i: Int) = i
        /** v */
        val v = 1
        /** u */
        var u = 2
      }     
      /** trait B */
      trait B {
        /** T */
        type T
        /** f */
        def f(i: Int)
        /** v */
        val v = 1
        /** u */
        var u = 2
      }     
      /** object C */
      object C {
        /** f */
        def f(i: Int) = i
        /** v */
        val v = 1
        /** u */
        var u = 2
      }
      /** class D */
      @deprecated("use ... instead", "2.10.0")
      class D
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
    // we want the Scaladoc compiler here, because it keeps DocDef nodes in the tree
    val settings = new Settings(_ => ())
    val command = new ScalaDoc.Command((CommandLineParser tokenize extraSettings) ++ args.toList, settings)
    new DocFactory(new ConsoleReporter(settings), settings).compiler
  }

  override def isDebug = false // so we don't get the newSettings warning
}
