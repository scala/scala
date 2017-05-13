package scala.tools.nsc

import scala.tools.nsc.interpreter._

// A compatibility stub for sbt
@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
class Interpreter(settings: Settings) extends IMain(settings)
