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

package scala

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._

/** The package object `scala.sys` contains methods for reading
 *  and altering core aspects of the virtual machine as well as the
 *  world outside of it.
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

  // TODO: consider whether layering a Map on top of Java's properties is really needed -- we could simply provide:
  //  def prop(p: String) = Option(System.getProperty(p))

  /** An immutable Map representing the current system environment.
   *
   *  If lookup fails, use `System.getenv(_)` for case-insensitive lookup
   *  on a certain platform. If that also fails, throw `NoSuchElementException`.
   *
   *  @return   a Map containing the system environment variables.
   */
  def env: Map[String, String] = Map.from(System.getenv().asScala).withDefault { v =>
    val s = System.getenv(v)
    if (s == null) throw new NoSuchElementException(v)
    s
  }

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

    ArraySeq.unsafeWrapArray(tarray).take(got)
  }
}
