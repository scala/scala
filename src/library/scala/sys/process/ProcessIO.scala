/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.sys
package process

import processInternal._

/** Each method will be called in a separate thread.*/
final class ProcessIO(
  val writeInput: OutputStream => Unit,
  val processOutput: InputStream => Unit,
  val processError: InputStream => Unit
) {
	def withInput(write: OutputStream => Unit): ProcessIO   = new ProcessIO(write, processOutput, processError)
	def withOutput(process: InputStream => Unit): ProcessIO = new ProcessIO(writeInput, process, processError)
	def withError(process: InputStream => Unit): ProcessIO  = new ProcessIO(writeInput, processOutput, process)
}
