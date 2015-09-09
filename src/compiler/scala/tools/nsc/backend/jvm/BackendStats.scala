/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm

import scala.reflect.internal.util.Statistics

// Enable with `-Ystatistics:jvm`
object BackendStats {
  import Statistics.{newTimer, newSubTimer}
  val bcodeTimer = newTimer("time in backend", "jvm")

  val bcodeInitTimer  = newSubTimer("bcode initialization", bcodeTimer)
  val bcodeGenStat    = newSubTimer("code generation", bcodeTimer)
  val methodOptTimer  = newSubTimer("intra-method optimizations", bcodeTimer)
  val bcodeWriteTimer = newSubTimer("classfile writing", bcodeTimer)

  def timed[T](timer: Statistics.Timer)(body: => T): T = {
    val start = Statistics.startTimer(timer)
    try body finally Statistics.stopTimer(timer, start)
  }
}
