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
package reporters

import scala.collection.mutable
import scala.reflect.internal.util.Position

/** This class implements a Reporter that stores its reports in the set `infos`. */
class StoreReporter extends Reporter {
  case class Info(pos: Position, msg: String, severity: Severity) {
    override def toString() = s"pos: $pos $msg $severity"
  }
  val infos = new mutable.LinkedHashSet[Info]
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    if (!force) {
      infos += Info(pos, msg, severity)
      count(severity)
    }
  }
  override def reset(): Unit = {
    super.reset()
    infos.clear()
  }
}
