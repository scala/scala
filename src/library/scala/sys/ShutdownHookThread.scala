/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package sys

/** A minimal Thread wrapper to enhance shutdown hooks.  It knows
 *  how to unregister itself.
 *
 *  @author Paul Phillips
 *  @version 2.9
 *  @since   2.9
 */
class ShutdownHookThread private (runnable: Runnable, name: String) extends Thread(runnable, name) {
  def remove() = Runtime.getRuntime removeShutdownHook this
}

object ShutdownHookThread {
  private var hookNameCount: Int = 0
  private def hookName(): String = synchronized {
    hookNameCount += 1
    "shutdownHook" + hookNameCount
  }
  /** Creates, names, and registers a shutdown hook to run the
   *  given code.
   */
  def apply(body: => Unit): ShutdownHookThread = {
    val t = new ShutdownHookThread(() => body, hookName)
    Runtime.getRuntime addShutdownHook t
    t
  }
}
