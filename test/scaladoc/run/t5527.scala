import scala.tools.nsc, nsc.doc
import scala.tools.nsc.doc.DocFactory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.partest.DirectTest
import scala.tools.partest.nest.StreamCapture
import scala.util.chaining._

object Test extends DirectTest {

  override def extraSettings = "-usejavacp -Vprint:parser -Yrangepos -Ystop-after:parser"

  override def code = """
    // scala/bug#5527
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

      def test5: Unit = {
        /** @martin is this right? It shouldn't flag me as scaladoc. */
        if (true) ???
      }

      def test6 = {
        /** Document this crucial constant for posterity.
         *  Don't forget to dedoc this comment if you refactor to a local.
         *  @author Paul Phillips
         */
        val u = 4
        for (i <- 0 to u)
          println(i)
      }
      def test7 = {
        /** Some standard tags are tolerated locally and shouldn't trigger a warning.
         *  @note Don't change this unless you know what you're doing. This means you.
         */
        val u = 4
        for (i <- 0 to u)
          println(i)
      }
      def test8 = {
        /*************************\
         * Fancy ASCII Art Block *
         *   @author som-snytt   *
        \*************************/
        // this is just a local
        val z = "fancy"
        z replace ("fanc", "arts")
      }
      def test9 = {
        val i = 10 */** Important!
                     *  We have to multiply here!
                     *  @author community
                     *  @see scala/bug#1234
                     */
                10
        assert(i == 100)
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
        def f(i: Int): Unit
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

      /** Get the simple value.
       *  @return the default value
       */
      // an intervening line comment
      /* I had more to say, but didn't want to pollute the scaladoc. */
      def value: Int = 7
    }
  """.trim

  override def show(): Unit = compile()

  // doc.Settings
  override def newSettings(args: List[String]) = new doc.Settings(_ => ()).tap(_.processArguments(args, true))
  // ScaladocGlobal yielded by DocFactory#compiler, requires doc.Settings
  // we want the Scaladoc compiler here, because it keeps DocDef nodes in the tree
  override def newCompiler(settings: nsc.Settings) = new DocFactory(reporter(settings), settings.asInstanceOf[doc.Settings]).compiler
}
