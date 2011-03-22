/* NSC -- new Scala compiler
 * Copyright 2009-2011 Scala Solutions and LAMP/EPFL
 * @author Martin Odersky
 * @author Iulian Dragos
 */
package scala.tools.nsc.interactive

/** A presentation compiler thread. This is a lightweight class, delegating most
 *  of its functionality to the compiler instance.
 *
 *  @note This thread class may not be GCd, so it's important not to keep around
 *        large objects. For instance, the JDT weaving framework keeps threads around
 *        in a map, preventing them from being GCd. This prompted the separation between
 *        interactive.Global and this class.
 */
class PresentationCompilerThread(var compiler: Global, threadId: Int) extends Thread("Scala Presentation Compiler V"+threadId) {
  /** The presentation compiler loop.
   */
  override def run() {
    compiler.debugLog("starting new runner thread")
    try {
      while (true) {
        compiler.log.logreplay("wait for more work", { compiler.scheduler.waitForMoreWork(); true })
        compiler.pollForWork(compiler.NoPosition)
        while (compiler.isOutOfDate) {
          try {
            compiler.backgroundCompile()
          } catch {
            case FreshRunReq =>
              compiler.debugLog("fresh run req caught, starting new pass")
          }
          compiler.log.flush()
        }
      }
    } catch {
      case ex @ ShutdownReq =>
        compiler.debugLog("exiting presentation compiler")
        compiler.log.close()

        // make sure we don't keep around stale instances
        compiler = null
      case ex =>
        compiler.log.flush()
        compiler.newRunnerThread()

        ex match {
          case FreshRunReq =>
            compiler.debugLog("fresh run req caught outside presentation compiler loop; ignored") // This shouldn't be reported
          case _ : Global#ValidateException => // This will have been reported elsewhere
            compiler.debugLog("validate exception caught outside presentation compiler loop; ignored")
          case _ => ex.printStackTrace(); compiler.informIDE("Fatal Error: "+ex)
        }

        // make sure we don't keep around stale instances
        compiler = null
    }
  }
}
