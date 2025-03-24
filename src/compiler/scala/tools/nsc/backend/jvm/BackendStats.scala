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
package backend.jvm

import scala.reflect.internal.util.Statistics

// Enable with `-Ystatistics:jvm`
trait BackendStats {
  self: Statistics =>

  val bcodeTimer      = newTimer("time in backend", "jvm")
  val bcodeInitTimer  = newSubTimer("bcode initialization", bcodeTimer)
  val bcodeGenStat    = newSubTimer("code generation", bcodeTimer)
  val methodOptTimer  = newSubTimer("intra-method optimizations", bcodeTimer)
  val bcodeWriteTimer = newSubTimer("classfile writing", bcodeTimer)
}
