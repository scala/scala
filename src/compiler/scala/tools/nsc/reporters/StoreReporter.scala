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
import scala.reflect.internal.util.Position

/** This class implements a Reporter that stores its reports in the set `infos`. */
class StoreReporter(val settings: Settings) extends FilteringReporter {
  @deprecated("use the constructor with a `Settings` parameter", since = "2.13.1")
  def this() = this(new Settings())

  @deprecated("use StoreReporter.Info", since = "2.13.0") // used in scalameta for example
  type Info = StoreReporter.Info

  @deprecated("use StoreReporter.Info", since = "2.13.0")
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
  import scala.reflect.internal.Reporter.Severity
  case class Info(pos: Position, msg: String, severity: Severity) {
    override def toString: String = s"pos: $pos $msg $severity"
  }
}
