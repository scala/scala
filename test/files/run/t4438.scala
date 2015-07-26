

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.{ IMain, PresentationCompilerCompleter }

import java.io.{ PrintWriter, StringWriter }


object Test extends App {
  // completion hangs off of JLine, which we lack in Testland. Note the trailing tab.
  def session =
    s"""|
        |scala> Array(1, 2, 3) rev\u0009
        | reverseIterator   reverse   reverseMap   reversed
        |
        |scala> :quit"""

  val settings = new Settings()
  settings.Xnojline.value = true
  settings.usejavacp.value = true

  val writer = new StringWriter
  val out = new PrintWriter(writer)

  val intp = new IMain(settings, out)

  val completer = new PresentationCompilerCompleter(intp)

  def `pcc handles extension methods of primitive arrays`() = {
    val code = "Array(1, 2, 3) rev"
    val candidates = completer.complete(code, code.length)

    assert(candidates.candidates.size == 5)
    assert((candidates.candidates.distinct.sorted mkString ", ") == "reverse, reverseIterator, reverseMap, reversed")
  }

  `pcc handles extension methods of primitive arrays`()
}
