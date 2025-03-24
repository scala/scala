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
package util.control

/**
 * Extractor of non-fatal Throwables. Will not match fatal errors like `VirtualMachineError`
 * (for example, `OutOfMemoryError` and `StackOverflowError`, subclasses of `VirtualMachineError`), `ThreadDeath`,
 * `LinkageError`, `InterruptedException`, `ControlThrowable`.
 *
 * Note that [[scala.util.control.ControlThrowable]], an internal Throwable, is not matched by
 * `NonFatal` (and would therefore be thrown).
 *
 * For example, all harmless Throwables can be caught by:
 * {{{
 *   try {
 *     // dangerous stuff
 *   } catch {
 *     case NonFatal(e) => log.error(e, "Something not that bad.")
 *    // or
 *     case e if NonFatal(e) => log.error(e, "Something not that bad.")
 *   }
 * }}}
 */
object NonFatal {
  /**
   * Returns true if the provided `Throwable` is to be considered non-fatal, or false if it is to be considered fatal
   */
  @annotation.nowarn("cat=deprecation")  // avoid warning on mention of ThreadDeath
  def apply(t: Throwable): Boolean = t match {
    // VirtualMachineError includes OutOfMemoryError and other fatal errors
    case _: VirtualMachineError | _: ThreadDeath | _: InterruptedException | _: LinkageError | _: ControlThrowable => false
    case _ => true
  }
  /**
   * Returns Some(t) if NonFatal(t) == true, otherwise None
   */
  def unapply(t: Throwable): Option[Throwable] = if (apply(t)) Some(t) else None
}
