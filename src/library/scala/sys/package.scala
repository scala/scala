/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.collection.immutable
import scala.collection.JavaConverters._

/** The package object `scala.sys` contains methods for reading
 *  and altering core aspects of the virtual machine as well as the
 *  world outside of it.
 *
 *  @author Paul Phillips
 *  @version 2.9
 *  @since   2.9
 */
package object sys {
  /** Throw a new RuntimeException with the supplied message.
   *
   *  @return   Nothing.
   */
  def error(message: String): Nothing = throw new RuntimeException(message)

  /** Exit the JVM with the default status code.
   *
   *  @return   Nothing.
   */
  def exit(): Nothing = exit(0)

  /** Exit the JVM with the given status code.
   *
   *  @return   Nothing.
   */
  def exit(status: Int): Nothing = {
    java.lang.System.exit(status)
    throw new Throwable()
  }

  /** A convenience method to get the current Runtime instance.
   *
   *  @return   the result of `java.lang.Runtime.getRuntime()`
   */
  def runtime: Runtime = Runtime.getRuntime

  /** A bidirectional, mutable Map representing the current system Properties.
   *
   *  @return   a SystemProperties.
   *  @see      [[scala.sys.SystemProperties]]
   */
  def props: SystemProperties = new SystemProperties

  /** An immutable Map representing the current system environment.
   *
   *  @return   a Map containing the system environment variables.
   */
  def env: immutable.Map[String, String] = immutable.Map(System.getenv().asScala.toSeq: _*)

  /** Register a shutdown hook to be run when the VM exits.
   *  The hook is automatically registered: the returned value can be ignored,
   *  but is available in case the Thread requires further modification.
   *  It can also be unregistered by calling ShutdownHookThread#remove().
   *
   *  Note that shutdown hooks are NOT guaranteed to be run.
   *
   *  @param    body  the body of code to run at shutdown
   *  @return   the   Thread which will run the shutdown hook.
   *  @see      [[scala.sys.ShutdownHookThread]]
   */
  def addShutdownHook(body: => Unit): ShutdownHookThread = ShutdownHookThread(body)

  /** Returns all active thread in the current thread's thread group and subgroups.
   *
   *  @return   an IndexedSeq containing the threads.
   */
  def allThreads(): IndexedSeq[Thread] = {
    val num    = Thread.activeCount()
    val tarray = new Array[Thread](num)
    val got    = Thread.enumerate(tarray)

    tarray take got
  }
}
