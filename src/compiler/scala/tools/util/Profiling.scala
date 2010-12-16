/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package util

/** This is a (very) minimal stub for profiling, the purpose
 *  of which is making it possible to integrate profiling hooks in
 *  the compiler without creating a dependency on any particular
 *  profiler.  You can specify a profiler class (which must be an
 *  instance of this class) like so:
 *
 *    // or -Yprofile:phase to profile individual phases
 *    scalac -Yprofile-class your.profiler.Class -Yprofile:all <files>
 *
 */
abstract class Profiling {
  def isActive: Boolean
  def startProfiling(): Unit
  def stopProfiling(): Unit
  def captureSnapshot(): Unit

  def profile[T](body: => T): T = {
    startProfiling()
    val result = body
    stopProfiling()
    captureSnapshot()
    result
  }

  /** Advance the current object generation.
   *
   *  Each object on the heap is associated to a generation number. Generations
   *  start at 1, and are automatically advanced on each snapshot capture.
   */
  def advanceGeneration(desc: String = ""): Unit
}
