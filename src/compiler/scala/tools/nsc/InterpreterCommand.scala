package scala.tools.nsc

import interpreter._

/** A compatibility stub.
 */
@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
class InterpreterCommand(arguments: List[String], error: String => Unit) extends CommandLine(arguments, error) { }