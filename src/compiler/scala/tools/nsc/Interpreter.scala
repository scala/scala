package scala.tools.nsc

import interpreter._
import java.io._

/** A compatibility stub.
 */
@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
class Interpreter(settings: Settings, out: PrintWriter) extends IMain(settings, out) {
  def this(settings: Settings) = this(settings, new NewLinePrintWriter(new ConsoleWriter, true))
  def this() = this(new Settings())
}