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

package scala
package sys

/** A minimal Thread wrapper to enhance shutdown hooks.  It knows
 *  how to unregister itself.
 *
 *  @author Paul Phillips
 *  @since   2.9
 */
class ShutdownHookThread private (name: String) extends Thread(name) {
  def remove() = runtime removeShutdownHook this
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
    val t = new ShutdownHookThread(hookName()) {
      override def run() = body
    }
    runtime addShutdownHook t
    t
  }
}
