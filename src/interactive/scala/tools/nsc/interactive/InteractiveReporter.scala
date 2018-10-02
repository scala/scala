/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package interactive

import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.util.Position
import scala.reflect.internal.Reporter

case class Problem(pos: Position, msg: String, severityLevel: Int)

abstract class InteractiveReporter extends Reporter {

  def compiler: Global

  val otherProblems = new ArrayBuffer[Problem]

  override def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = try {
    severity.count += 1
    val problems =
      if (compiler eq null) {
        otherProblems
      } else if (pos.isDefined) {
        compiler.getUnit(pos.source) match {
          case Some(unit) =>
            compiler.debugLog(pos.source.file.name + ":" + pos.line + ": " + msg)
            unit.problems
          case None =>
            compiler.debugLog(pos.source.file.name + "[not loaded] :" + pos.line + ": " + msg)
            otherProblems
        }
      } else {
        compiler.debugLog("[no position] :" + msg)
        otherProblems
      }
    problems += Problem(pos, msg, severity.id)
  } catch {
    case ex: UnsupportedOperationException =>
  }

  override def reset(): Unit = {
    super.reset()
    otherProblems.clear()
  }
}
