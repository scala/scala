
import scala.util.{ Try, Success, Failure }
import xml._
import scala.io.Source.fromString
import java.io.PrintStream

object Test extends App {

  def quietSource(text: String) = new io.Source {
    override protected val iter = io.Source fromString text
    override def report(pos: Int, msg: String, out: PrintStream) = ()
  }
  def reading(text: String)(f: pull.XMLEventReader => Unit): Unit = {
    val r = new pull.XMLEventReader(quietSource(text))
    try f(r)
    finally r.stop()
  }
  def trying(body: => Unit): Unit =
    Try (body) match {
      case Success(_) => Console println "Expected failure"
      case Failure(e) => Console println s"Saw failure: $e"
    }

  val problematic = """<foo bar="baz/>"""

  trying (
    parsing.ConstructingParser.fromSource(quietSource(problematic), false).document.docElem
  )

  trying (
    reading(problematic) { r =>
      while (r.hasNext) println(r.next())
    }
  )
}
