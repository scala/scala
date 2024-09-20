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

package scala.tools.nsc.interactive

import scala.util.chaining._

/** A presentation compiler thread. This is a lightweight class, delegating most
 *  of its functionality to the compiler instance.
 *
 */
final class PresentationCompilerThread private (task: Runnable, name: String) extends Thread(task, name)

object PresentationCompilerThread {

  def apply(compiler: Global, name: String) =
    new PresentationCompilerThread(new Task(compiler), s"Scala Presentation Compiler [$name]")
      .tap(_.setDaemon(true))

  private class Task(compiler: Global) extends Runnable {

    /** The presentation compiler loop.
     */
    override def run(): Unit = {
      compiler.debugLog("starting new runner thread")
      def loop(): Unit =
        try {
          compiler.checkNoResponsesOutstanding()
          compiler.log.logreplay("wait for more work", { compiler.scheduler.waitForMoreWork(); true })
          compiler.pollForWork(compiler.NoPosition)
          while (compiler.isOutOfDate) {
            try compiler.backgroundCompile()
            catch {
              case ex: FreshRunReq => compiler.debugLog("fresh run req caught, starting new pass")
            }
            compiler.log.flush()
          }
          loop()
        } catch {
          case ShutdownReq =>
            compiler.debugLog("exiting presentation compiler")
            compiler.log.close()
          case ex: FreshRunReq =>
            compiler.log.flush()
            compiler.debugLog("fresh run req caught outside presentation compiler loop; ignored")
              // This shouldn't be reported
            loop()
          case _ : Global#ValidateException =>
            compiler.log.flush()
            compiler.debugLog("validate exception caught outside presentation compiler loop; ignored")
              // This will have been reported elsewhere
            loop()
          case ex: RuntimeException =>
            compiler.log.flush()
            ex.printStackTrace()
            compiler.informIDE(s"Fatal Error: $ex")
            compiler.log.close()
        }
      loop()
    }
  }
}
