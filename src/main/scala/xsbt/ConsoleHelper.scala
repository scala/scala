package xsbtpamflet

import scala.tools.nsc.interpreter.IR
import xsbti.ConsoleResult

object ConsoleHelper {
  implicit def toConsoleResult(ir: IR.Result): ConsoleResult =
    ir match {
      case IR.Success    => ConsoleResult.Success
      case IR.Incomplete => ConsoleResult.Incomplete
      case IR.Error      => ConsoleResult.Error
    }
}
