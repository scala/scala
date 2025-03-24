/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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
import scala.reflect.internal.util.{CodeAction, Position}

/** This class implements a Reporter that stores its reports in the set `infos`. */
class StoreReporter(val settings: Settings) extends FilteringReporter {
  @deprecated("use the constructor with a `Settings` parameter", since = "2.13.1")
  def this() = this(new Settings())

  @deprecated("use StoreReporter.Info", since = "2.13.0") // used in scalameta for example
  type Info = StoreReporter.Info

  @deprecated("use StoreReporter.Info", since = "2.13.0")
  @uncheckedStable def Info: StoreReporter.Info.type = StoreReporter.Info

  val infos = new mutable.LinkedHashSet[StoreReporter.Info]

  override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit = {
    val info = StoreReporter.Info(pos, msg, severity, actions)
    infos += info
  }

  override def reset(): Unit = {
    super.reset()
    infos.clear()
  }
}
object StoreReporter {
  case class Info(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]) {
    override def toString: String = s"pos: $pos $msg $severity${if (actions.isEmpty) "" else actions}"
  }
}
