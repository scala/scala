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

import scala.annotation.unchecked.uncheckedStable
import scala.collection.mutable
import scala.reflect.internal.Reporter.Severity
import scala.reflect.internal.util.Position

/** This class implements a Reporter that stores its reports in the set `infos`. */
class StoreReporter(val settings: Settings) extends FilteringReporter {
  @deprecated("use the constructor with a `Settings` parameter", "2.12.13")
  def this() = this(new Settings())

  @deprecated("use StoreReporter.Info") // used in scalameta for example
  type Info = StoreReporter.Info

  @deprecated("use StoreReporter.Info")
  @uncheckedStable def Info: StoreReporter.Info.type = StoreReporter.Info

  val infos = new mutable.LinkedHashSet[StoreReporter.Info]

  def doReport(pos: Position, msg: String, severity: Severity): Unit =
    infos += StoreReporter.Info(pos, msg, severity)

  override def reset(): Unit = {
    super.reset()
    infos.clear()
  }
}
object StoreReporter {
  case class Info(pos: Position, msg: String, severity: Severity) {
    override def toString: String = s"pos: $pos $msg $severity"
  }
}
