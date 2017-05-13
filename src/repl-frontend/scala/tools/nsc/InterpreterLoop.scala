package scala.tools.nsc

import scala.tools.nsc.interpreter.InteractiveReader

// A compatibility stub for sbt
@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
class InterpreterLoop extends interpreter.shell.ILoop {
  @deprecated("Unused.", "2.9.0")
  var in: InteractiveReader.type = InteractiveReader
}
