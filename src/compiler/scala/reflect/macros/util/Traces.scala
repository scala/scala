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

package scala.reflect.macros
package util

trait Traces {
  def globalSettings: scala.tools.nsc.Settings

  val macroDebugLite = globalSettings.YmacrodebugLite.value
  val macroDebugVerbose = globalSettings.YmacrodebugVerbose.value
  @inline final def macroLogLite(msg: => Any): Unit = { if (macroDebugLite || macroDebugVerbose) println(msg) }
  @inline final def macroLogVerbose(msg: => Any): Unit = { if (macroDebugVerbose) println(msg) }
}
