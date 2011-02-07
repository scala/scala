package scala.tools.nsc
package interactive

import collection.mutable.ArrayBuffer
import util.Position
import reporters.Reporter

trait Reporters { self: Global =>

  case class Problem(pos: Position, msg: String, severity: Int)

  val otherProblems = new ArrayBuffer[Problem]

  class InteractiveReporter extends Reporter {
    override def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
      severity.count += 1
      val problems =
        if (pos.isDefined) {
          getUnit(pos.source) match {
            case Some(unit) =>
              debugLog(pos.source.file.name + ":" + pos.line + ": " + msg)
              unit.problems
            case None =>
              debugLog(pos.source.file.name + "[not loaded] :" + pos.line + ": " + msg)
              otherProblems
          }
        } else {
          debugLog("[no position] :" + msg)
          otherProblems
        }
      problems += Problem(pos, msg, severity.id)
    }

    override def reset() {
      super.reset()
      otherProblems.clear()
    }
  }
}