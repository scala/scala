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
package sys
package process

import processInternal._

/** This class is used to control the I/O of every
  * [[scala.sys.process.Process]]. The functions used to create it will be
  * called with the process streams once it has been started. It might not be
  * necessary to use `ProcessIO` directly --
  * [[scala.sys.process.ProcessBuilder]] can return the process output to the
  * caller, or use a [[scala.sys.process.ProcessLogger]] which avoids direct
  * interaction with a stream. One can even use the factories at `BasicIO` to
  * create a `ProcessIO`, or use its helper methods when creating one's own
  * `ProcessIO`.
  *
  * When creating a `ProcessIO`, it is important to ''close all streams'' when
  * finished, since the JVM might use system resources to capture the process
  * input and output, and will not release them unless the streams are
  * explicitly closed.
  *
  * `ProcessBuilder` will call `writeInput`, `processOutput` and `processError`
  * in separate threads, and if daemonizeThreads is true, they will all be
  * marked as daemon threads.
  *
  * @param writeInput Function that will be called with the `OutputStream` to
  *                   which all input to the process must be written. This will
  *                   be called in a newly spawned thread.
  * @param processOutput Function that will be called with the `InputStream`
  *                      from which all normal output of the process must be
  *                      read from. This will be called in a newly spawned
  *                      thread.
  * @param processError Function that will be called with the `InputStream` from
  *                     which all error output of the process must be read from.
  *                     This will be called in a newly spawned thread.
  * @param daemonizeThreads Indicates whether the newly spawned threads that
  *                         will run `processOutput`, `processError` and
  *                         `writeInput` should be marked as daemon threads.
  * @note Failure to close the passed streams may result in resource leakage.
  */
final class ProcessIO(
  val writeInput: OutputStream => Unit,
  val processOutput: InputStream => Unit,
  val processError: InputStream => Unit,
  val daemonizeThreads: Boolean
) {
  def this(in: OutputStream => Unit, out: InputStream => Unit, err: InputStream => Unit) = this(in, out, err, daemonizeThreads = false)

  /** Creates a new `ProcessIO` with a different handler for the process input. */
  def withInput(write: OutputStream => Unit): ProcessIO   = new ProcessIO(write, processOutput, processError, daemonizeThreads)

  /** Creates a new `ProcessIO` with a different handler for the normal output. */
  def withOutput(process: InputStream => Unit): ProcessIO = new ProcessIO(writeInput, process, processError, daemonizeThreads)

  /** Creates a new `ProcessIO` with a different handler for the error output. */
  def withError(process: InputStream => Unit): ProcessIO  = new ProcessIO(writeInput, processOutput, process, daemonizeThreads)

  /** Creates a new `ProcessIO`, with `daemonizeThreads` true. */
  def daemonized(): ProcessIO = new ProcessIO(writeInput, processOutput, processError, daemonizeThreads = true)
}
