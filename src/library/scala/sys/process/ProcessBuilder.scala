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
import ProcessBuilder._

/** Represents a runnable process.
  *
  * This is the main component of this package. A `ProcessBuilder` may be composed with
  * others, either concatenating their outputs or piping them from one to the next, and
  * possibly with conditional execution depending on the last process exit value.
  *
  * Once executed, one can retrieve the output or redirect it to a
  * [[scala.sys.process.ProcessLogger]], or one can get the exit value, discarding or
  * redirecting the output.
  *
  * One creates a `ProcessBuilder` through factories provided in [[scala.sys.process.Process]]'s
  * companion object, or implicit conversions based on these factories made available in the
  * package object [[scala.sys.process]].
  *
  * Let's examine in detail one example of usage:
  *
  * {{{
  * import scala.sys.process._
  * "find src -name *.scala -exec grep null {} ;"  #|  "xargs test -z"  #&&  "echo null-free"  #||  "echo null detected"  !
  * }}}
  *
  * Note that every `String` is implicitly converted into a `ProcessBuilder`
  * through the implicits imported from [[scala.sys.process]]. These `ProcessBuilder` are then
  * combined in three different ways.
  *
  *   1. `#|` pipes the output of the first command into the input of the second command. It
  * mirrors a shell pipe (`|`).
  *   2. `#&&` conditionally executes the second command if the previous one finished with
  * exit value 0. It mirrors shell's `&&`.
  *   3. `#||` conditionally executes the third command if the exit value of the previous
  * command is different than zero. It mirrors shell's `&&`.
  *
  * Not shown here, the equivalent of a shell's `;` would be `###`. The reason for this name is
  * that `;` is a reserved token in Scala.
  *
  * Finally, `!` at the end executes the commands, and returns the exit value. If the output
  * was desired instead, one could run that with `!!` instead.
  *
  * If one wishes to execute the commands in background, one can either call `run`, which
  * returns a [[scala.sys.process.Process]] from which the exit value can be obtained, or
  * `lines`, which returns a [scala.collection.immutable.Stream] of output lines. This throws
  * an exception at the end of the `Stream` is the exit value is non-zero. To avoid exceptions,
  * one can use `lines_!` instead.
  *
  * One can also start the commands in specific ways to further control their I/O. Using `!<` to
  * start the commands will use the stdin from the current process for them. All methods can
  * be used passing a [[scala.sys.process.ProcessLogger]] to capture the output, both stderr and
  * stdout. And, when using `run`, one can pass a [[scala.sys.process.ProcessIO]] to control
  * stdin, stdout and stderr.
  *
  * The stdin of a command can be redirected from a `java.io.InputStream`, a `java.io.File`, a
  * `java.net.URL` or another `ProcessBuilder` through the method `#<`. Likewise, the stdout
  * can be sent to a `java.io.OutputStream`, a `java.io.File` or another `ProcessBuilder` with
  * the method `#>`. The method `#>>` can be used to append the output to a `java.io.File`.
  * For example:
  *
  * {{{
  * new URL("http://databinder.net/dispatch/About") #> "grep JSON" #>> new File("About_JSON") !
  * }}}
  */
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

  /** True if this command can be the target of a pipe.
   */
  def canPipeTo: Boolean

  /** True if this command has an exit code which should be propagated to the user.
   *  Given a pipe between A and B, if B.hasExitValue is true then the exit code will
   *  be the one from B; if it is false, the one from A.  This exists to prevent output
   *  redirections (implemented as pipes) from masking useful process error codes.
   */
  def hasExitValue: Boolean
}

/** This object contains traits used to describe input and output sources. */
object ProcessBuilder extends ProcessBuilderImpl {
  /** Used when creating [[scala.sys.process.ProcessBuilder.Source]] from an URL. */
  trait URLBuilder extends Source {

  }

  /** Used when creating [[scala.sys.process.ProcessBuilder.Source]] and/or
    * [[scala.sys.process.ProcessBuilder.Sink]] from a file.
    */
  trait FileBuilder extends Sink with Source {
    /** Append the contents of a `java.io.File` to this file */
    def #<<(f: File): ProcessBuilder

    /** Append the contents from a `java.net.URL` to this file */
    def #<<(u: URL): ProcessBuilder

    /** Append the contents of a `java.io.InputStream` to this file */
    def #<<(i: => InputStream): ProcessBuilder

    /** Append the contents of a [[scala.sys.process.ProcessBuilder]] to this file */
    def #<<(p: ProcessBuilder): ProcessBuilder
  }

  /** Represents everything that can be used as an input to a
    * [[scala.sys.process.ProcessBuilder]].
    */
  trait Source {
    protected def toSource: ProcessBuilder

    /** Writes the output stream of this process to the given file. */
    def #> (f: File): ProcessBuilder = toFile(f, false)

    /** Appends the output stream of this process to the given file. */
    def #>> (f: File): ProcessBuilder = toFile(f, true)

    /** Writes the output stream of this process to the given OutputStream. The
      * argument is call-by-name, so the stream is recreated, written, and closed each
      * time this process is executed.
      */
    def #>(out: => OutputStream): ProcessBuilder = #> (new OStreamBuilder(out, "<output stream>"))

    /** Writes the output stream of this process to a [[scala.sys.process.ProcessBuilder]]. */
    def #>(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(toSource, b, false)

    /** Returnes a [[scala.sys.process.ProcessBuilder]] representing this `Source`. */
    def cat = toSource
    private def toFile(f: File, append: Boolean) = #> (new FileOutput(f, append))
  }

  /** Represents everything that can receive an output from a
    * [[scala.sys.process.ProcessBuilder]].
    */
  trait Sink {
    protected def toSink: ProcessBuilder

    /** Reads the given file into the input stream of this process. */
    def #< (f: File): ProcessBuilder = #< (new FileInput(f))

    /** Reads the given URL into the input stream of this process. */
    def #< (f: URL): ProcessBuilder = #< (new URLInput(f))

    /** Reads the given InputStream into the input stream of this process. The
      * argument is call-by-name, so the stream is recreated, read, and closed each
      * time this process is executed.
      */
    def #<(in: => InputStream): ProcessBuilder = #< (new IStreamBuilder(in, "<input stream>"))

    /** Reads the output of a [[scala.sys.process.ProcessBuilder]] into the input stream of this process. */
    def #<(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(b, toSink, false)
  }
}
