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
import ProcessBuilder._

/** Represents a runnable process. */
trait ProcessBuilder extends Source with Sink {
	/** Starts the process represented by this builder, blocks until it exits, and returns the output as a String.  Standard error is
	* sent to the console.  If the exit code is non-zero, an exception is thrown.*/
	def !! : String
	/** Starts the process represented by this builder, blocks until it exits, and returns the output as a String.  Standard error is
	* sent to the provided ProcessLogger.  If the exit code is non-zero, an exception is thrown.*/
	def !!(log: ProcessLogger): String
	/** Starts the process represented by this builder.  The output is returned as a Stream that blocks when lines are not available
	* but the process has not completed.  Standard error is sent to the console.  If the process exits with a non-zero value,
	* the Stream will provide all lines up to termination and then throw an exception. */
	def lines: Stream[String]
	/** Starts the process represented by this builder.  The output is returned as a Stream that blocks when lines are not available
	* but the process has not completed.  Standard error is sent to the provided ProcessLogger.  If the process exits with a non-zero value,
	* the Stream will provide all lines up to termination but will not throw an exception. */
	def lines(log: ProcessLogger): Stream[String]
	/** Starts the process represented by this builder.  The output is returned as a Stream that blocks when lines are not available
	* but the process has not completed.  Standard error is sent to the console. If the process exits with a non-zero value,
	* the Stream will provide all lines up to termination but will not throw an exception. */
	def lines_! : Stream[String]
	/** Starts the process represented by this builder.  The output is returned as a Stream that blocks when lines are not available
	* but the process has not completed.  Standard error is sent to the provided ProcessLogger. If the process exits with a non-zero value,
	* the Stream will provide all lines up to termination but will not throw an exception. */
	def lines_!(log: ProcessLogger): Stream[String]
	/** Starts the process represented by this builder, blocks until it exits, and returns the exit code.  Standard output and error are
	* sent to the console.*/
	def ! : Int
	/** Starts the process represented by this builder, blocks until it exits, and returns the exit code.  Standard output and error are
	* sent to the given ProcessLogger.*/
	def !(log: ProcessLogger): Int
	/** Starts the process represented by this builder, blocks until it exits, and returns the exit code.  Standard output and error are
	* sent to the console.  The newly started process reads from standard input of the current process.*/
	def !< : Int
	/** Starts the process represented by this builder, blocks until it exits, and returns the exit code.  Standard output and error are
	* sent to the given ProcessLogger.  The newly started process reads from standard input of the current process.*/
	def !<(log: ProcessLogger): Int
	/** Starts the process represented by this builder.  Standard output and error are sent to the console.*/
	def run(): Process
	/** Starts the process represented by this builder.  Standard output and error are sent to the given ProcessLogger.*/
	def run(log: ProcessLogger): Process
	/** Starts the process represented by this builder.  I/O is handled by the given ProcessIO instance.*/
	def run(io: ProcessIO): Process
	/** Starts the process represented by this builder.  Standard output and error are sent to the console.
	* The newly started process reads from standard input of the current process if `connectInput` is true.*/
	def run(connectInput: Boolean): Process
	/** Starts the process represented by this builder, blocks until it exits, and returns the exit code.  Standard output and error are
	* sent to the given ProcessLogger.
	* The newly started process reads from standard input of the current process if `connectInput` is true.*/
	def run(log: ProcessLogger, connectInput: Boolean): Process

	/** Constructs a command that runs this command first and then `other` if this command succeeds.*/
	def #&& (other: ProcessBuilder): ProcessBuilder
	/** Constructs a command that runs this command first and then `other` if this command does not succeed.*/
	def #|| (other: ProcessBuilder): ProcessBuilder
	/** Constructs a command that will run this command and pipes the output to `other`.  `other` must be a simple command.*/
	def #| (other: ProcessBuilder): ProcessBuilder
	/** Constructs a command that will run this command and then `other`.  The exit code will be the exit code of `other`.*/
	def ### (other: ProcessBuilder): ProcessBuilder

	def canPipeTo: Boolean
}

object ProcessBuilder extends ProcessBuilderImpl {
  trait URLBuilder extends Source {

  }
  trait FileBuilder extends Sink with Source {
  	def #<<(f: File): ProcessBuilder
  	def #<<(u: URL): ProcessBuilder
  	def #<<(i: => InputStream): ProcessBuilder
  	def #<<(p: ProcessBuilder): ProcessBuilder
  }
  trait Source {
    protected def toSource: ProcessBuilder
  	/** Writes the output stream of this process to the given file. */
  	def #> (f: File): ProcessBuilder = toFile(f, false)
  	/** Appends the output stream of this process to the given file. */
  	def #>> (f: File): ProcessBuilder = toFile(f, true)
  	/** Writes the output stream of this process to the given OutputStream. The
  	* argument is call-by-name, so the stream is recreated, written, and closed each
  	* time this process is executed. */
  	def #>(out: => OutputStream): ProcessBuilder = #> (new OStreamBuilder(out, "<output stream>"))
  	def #>(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(toSource, b, false)
  	def cat = toSource
  	private def toFile(f: File, append: Boolean) = #> (new FileOutput(f, append))
  }
  trait Sink {
  	protected def toSink: ProcessBuilder
  	/** Reads the given file into the input stream of this process. */
  	def #< (f: File): ProcessBuilder = #< (new FileInput(f))
  	/** Reads the given URL into the input stream of this process. */
  	def #< (f: URL): ProcessBuilder = #< (new URLInput(f))
  	/** Reads the given InputStream into the input stream of this process. The
  	* argument is call-by-name, so the stream is recreated, read, and closed each
  	* time this process is executed. */
  	def #<(in: => InputStream): ProcessBuilder = #< (new IStreamBuilder(in, "<input stream>"))
  	def #<(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(b, toSink, false)
  }
}
