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

package scala.tools.nsc.interactive

import scala.annotation.nowarn

/** A presentation compiler thread. This is a lightweight class, delegating most
 *  of its functionality to the compiler instance.
 *
 */
final class PresentationCompilerThread(var compiler: Global, name: String = "")
  extends Thread("Scala Presentation Compiler [" + name + "]") {

  /** The presentation compiler loop.
   */
  override def run(): Unit = {
    compiler.debugLog("starting new runner thread")
    while (compiler ne null) try {
      compiler.checkNoResponsesOutstanding()
      compiler.log.logreplay("wait for more work", { compiler.scheduler.waitForMoreWork(); true }): @nowarn("cat=w-flag-value-discard")
      compiler.pollForWork(compiler.NoPosition)
      while (compiler.isOutOfDate) {
        try {
          compiler.backgroundCompile()
        } catch {
          case ex: FreshRunReq =>
            compiler.debugLog("fresh run req caught, starting new pass")
        }
        compiler.log.flush()
      }
    } catch {
      case ex @ ShutdownReq =>
        compiler.debugLog("exiting presentation compiler")
        compiler.log.close()

        // make sure we don't keep around stale instances
        compiler = null
      case ex: Throwable =>
        compiler.log.flush()

        ex match {
          case ex: FreshRunReq =>
            compiler.debugLog("fresh run req caught outside presentation compiler loop; ignored") // This shouldn't be reported
          case _ : Global#ValidateException => // This will have been reported elsewhere
            compiler.debugLog("validate exception caught outside presentation compiler loop; ignored")
          case _ => ex.printStackTrace(); compiler.informIDE("Fatal Error: "+ex)
        }
    }
  }
}
