package scala.tools.nsc

import interpreter._
import java.io._

/** A compatibility stub.
 */
@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
class InterpreterLoop(in0: Option[BufferedReader], out: PrintWriter) extends ILoop(in0, out) {
  def this(in0: BufferedReader, out: PrintWriter) = this(Some(in0), out)
  def this() = this(None, new PrintWriter(scala.Console.out))
}
