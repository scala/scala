/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm

import scala.reflect.internal.util.Statistics

object BackendStats {
  import Statistics.{newTimer, newSubTimer}
  val bcodeTimer = newTimer("time in backend", "jvm")

  val bcodeInitTimer  = newSubTimer("bcode initialization", bcodeTimer)
  val bcodeGenStat    = newSubTimer("code generation", bcodeTimer)
  val bcodeDceTimer   = newSubTimer("dead code elimination", bcodeTimer)
  val bcodeWriteTimer = newSubTimer("classfile writing", bcodeTimer)
}
