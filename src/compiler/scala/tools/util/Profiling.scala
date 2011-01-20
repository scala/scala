/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
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

  def allocationFreq: Option[Int] // record every Nth allocation
  def startRecordingAllocations(): Unit
  def stopRecordingAllocations(): Unit

  def profile[T](body: => T): T = profileCPU(body)

  def profileCPU[T](body: => T): T = {
    startProfiling()
    val result = body
    stopProfiling()
    captureSnapshot()
    result
  }

  def profileMem[T](body: => T): T = {
    startRecordingAllocations()
    val result = body
    stopRecordingAllocations()
    result
  }

  /** Advance the current object generation.
   *
   *  Each object on the heap is associated to a generation number. Generations
   *  start at 1, and are automatically advanced on each snapshot capture.
   */
  def advanceGeneration(desc: String = ""): Unit
}
