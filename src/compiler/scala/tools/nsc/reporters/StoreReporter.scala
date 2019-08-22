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
import scala.reflect.internal.Reporter.Severity
import scala.reflect.internal.util.Position

/** This class implements a Reporter that stores its reports in the set `infos`. */
class StoreReporter(val settings: Settings) extends FilteringReporter {
  import StoreReporter._

  def this() = this(new Settings)

  val infos = new mutable.LinkedHashSet[Info]

  override def doReport(pos: Position, msg: String, severity: Severity) = infos += Info(pos, msg, severity)

  override def reset(): Unit = {
    super.reset()
    infos.clear()
  }
}
object StoreReporter {
  case class Info(pos: Position, msg: String, severity: Severity) {
    override def toString() = s"pos: $pos $msg $severity"
  }
}
