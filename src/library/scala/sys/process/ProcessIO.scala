/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.sys
package process

import processInternal._

/** Each method will be called in a separate thread.
 *  If daemonizeThreads is true, they will all be marked daemon threads.
 */
final class ProcessIO(
  val writeInput: OutputStream => Unit,
  val processOutput: InputStream => Unit,
  val processError: InputStream => Unit,
  val daemonizeThreads: Boolean
) {
  def this(in: OutputStream => Unit, out: InputStream => Unit, err: InputStream => Unit) = this(in, out, err, false)

  def withInput(write: OutputStream => Unit): ProcessIO   = new ProcessIO(write, processOutput, processError, daemonizeThreads)
  def withOutput(process: InputStream => Unit): ProcessIO = new ProcessIO(writeInput, process, processError, daemonizeThreads)
  def withError(process: InputStream => Unit): ProcessIO  = new ProcessIO(writeInput, processOutput, process, daemonizeThreads)
  def daemonized(): ProcessIO = new ProcessIO(writeInput, processOutput, processError, true)
}
