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
package process

import processInternal._
import ProcessBuilder.{Sink, Source}

/** Represents a sequence of one or more external processes that can be
  * executed. A `ProcessBuilder` can be a single external process, or a
  * combination of other `ProcessBuilder`. One can control where the
  * output of an external process will go to, and where its input will come
  * from, or leave that decision to whoever starts it.
  *
  * One creates a `ProcessBuilder` through factories provided in
  * [[scala.sys.process.Process]]'s companion object, or implicit conversions
  * based on these factories made available in the package object
  * [[scala.sys.process]]. Here are some examples:
  * {{{
  * import scala.sys.process._
  *
  * // Executes "ls" and sends output to stdout
  * "ls".runBlocking
  *
  * // Execute "ls" and assign a `LazyList[String]` of its output to "contents".
  * val contents = Process("ls").lazyLines
  *
  * // Here we use a `Seq` to make the parameter whitespace-safe
  * def contentsOf(dir: String): String = Seq("ls", dir).outputString
  * }}}
  *
  * The methods of `ProcessBuilder` are divided in three categories: the ones that
  * combine two `ProcessBuilder` to create a third, the ones that redirect input
  * or output of a `ProcessBuilder`, and the ones that execute
  * the external processes associated with it.
  *
  * ==Combining `ProcessBuilder`==
  *
  * Two existing `ProcessBuilder` can be combined in the following ways:
  *
  *   - They can be executed in parallel, with the output of the first being fed
  *     as input to the second, like Unix pipes. This is achieved with the `pipeTo`
  *     method.
  *   - They can be executed in sequence, with the second starting as soon as
  *     the first ends. This is done by the `andThen` method.
  *   - The execution of the second one can be conditioned by the return code
  *     (exit status) of the first, either only when it's zero, or only when it's
  *     not zero. The methods `ifSuceedsThen` and `ifFailsThen` accomplish these tasks.
  *
  * ==Redirecting Input/Output==
  *
  * Though control of input and output can be done when executing the process,
  * there's a few methods that create a new `ProcessBuilder` with a
  * pre-configured input or output. They are `read, `overwrite` and `append`, and may take
  * as input either another `ProcessBuilder` (like the pipe described above), or
  * something else such as a `java.io.File` or a `java.io.InputStream`.
  * For example:
  * {{{
  * new URL("http://databinder.net/dispatch/About") overwrite "grep JSON" append new File("About_JSON") runBlocking
  * }}}
  *
  * ==Starting Processes==
  *
  * To execute all external commands associated with a `ProcessBuilder`, one
  * may use one of four groups of methods. Each of these methods have various
  * overloads and variations to enable further control over the I/O. These
  * methods are:
  *
  *   - `run`: the most general method, it returns a
  *     [[scala.sys.process.Process]] immediately, and the external command
  *     executes concurrently.
  *   - `runBlocking`: blocks until all external commands exit, and returns the exit code
  *     of the last one in the chain of execution.
  *   - `outputString`: blocks until all external commands exit, and returns a `String`
  *     with the output generated.
  *   - `lazyLines`: returns immediately like `run`, and the output being generated
  *     is provided through a `LazyList[String]`. Getting the next element of that
  *     `LazyList` may block until it becomes available. This method will throw an
  *     exception if the return code is different than zero -- if this is not
  *     desired, use the `lazyLines_!` method.
  *
  * ==Handling Input and Output==
  *
  * If not specified, the input of the external commands executed with `run` or
  * `runBlocking` will not be tied to anything, and the output will be redirected to the
  * stdout and stderr of the Scala process. For the methods `outputString` and `lazyLines`, no
  * input will be provided, and the output will be directed according to the
  * semantics of these methods.
  *
  * Some methods will cause stdin to be used as input. Output can be controlled
  * with a [[scala.sys.process.ProcessLogger]] -- `outputString` and `lazyLines` will only
  * redirect error output when passed a `ProcessLogger`. If one desires full
  * control over input and output, then a [[scala.sys.process.ProcessIO]] can be
  * used with `run`.
  *
  * For example, we could silence the error output from `lazyLines_!` like this:
  * {{{
  * val etcFiles = "find /etc" lazyLines_! ProcessLogger(line => ())
  * }}}
  *
  * ==Extended Example==
  *
  * Let's examine in detail one example of usage:
  * {{{
  * import scala.sys.process._
  * "find src -name *.scala -exec grep null {} ;" pipeTo "xargs test -z" ifSuceedsThen "echo null-free" ifFailsThen "echo null detected" runBlocking
  * }}}
  * Note that every `String` is implicitly converted into a `ProcessBuilder`
  * through the implicits imported from [[scala.sys.process]]. These `ProcessBuilder` are then
  * combined in three different ways.
  *
  *   1. `pipeTo` pipes the output of the first command into the input of the second command. It
  *      mirrors a shell pipe (`|`).
  *   1. `ifSuceedsThen` conditionally executes the second command if the previous one finished with
  *      exit value 0. It mirrors shell's `&&`.
  *   1. `ifFailsThen` conditionally executes the third command if the exit value of the previous
  *      command is different than zero. It mirrors shell's `||`.
  *
  * Finally, `runBlocking` at the end executes the commands, and returns the exit value.
  * Whatever is printed will be sent to the Scala process standard output. If
  * we wanted to capture it, we could run that with `outputString` instead.
  *
  * Note: though it is not shown above, the equivalent of a shell's `;` would be
  * `andThen`. The reason for this name is that `;` is a reserved token in Scala.
  *
  */
trait ProcessBuilder extends Source with Sink {
  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the output as a String.  Standard error is sent to the console.  If
    * the exit code is non-zero, an exception is thrown.
    */
  def outputString : String

  @deprecated("Use outputString", since = "2.13.0")
  def !! : String = outputString

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the output as a String.  Standard error is sent to the provided
    * ProcessLogger.  If the exit code is non-zero, an exception is thrown.
    */
  def outputString(log: ProcessLogger): String

  @deprecated("Use outputString", since = "2.13.0")
  def !!(log: ProcessLogger): String = outputString(log)

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the output as a String.  Standard error is sent to the console.  If
    * the exit code is non-zero, an exception is thrown.  The newly started
    * process reads from standard input of the current process.
    */
  def outputStringFromStdin : String

  @deprecated("Use outputStringFromStdin", since = "2.13.0")
  def !!< : String = outputStringFromStdin

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the output as a String.  Standard error is sent to the provided
    * ProcessLogger.  If the exit code is non-zero, an exception is thrown.  The
    * newly started process reads from standard input of the current process.
    */
  def outputStringFromStdin(log: ProcessLogger): String

  @deprecated("Use outputStringFromStdin", since = "2.13.0")
  def !!<(log: ProcessLogger): String = outputStringFromStdin(log)

  /** Starts the process represented by this builder.  The output is returned as
    * a LazyList that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the console.  If the process exits
    * with a non-zero value, the LazyList will provide all lines up to termination
    * and then throw an exception.
    */
  def lazyLines: LazyList[String]

  /** Starts the process represented by this builder.  The output is returned as
   * a LazyList that blocks when lines are not available but the process has not
   * completed.
   * The producer process will block if the given capacity of lines if filled
   * without being consumed from the LazyList.
   * Standard error is sent to the console.  If the process exits
   * with a non-zero value, the LazyList will provide all lines up to termination
   * and then throw an exception.
   */
  def lazyLines(capacity: Integer): LazyList[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a LazyList that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the provided ProcessLogger.  If the
    * process exits with a non-zero value, the LazyList will provide all lines up
    * to termination and then throw an exception.
    */
  def lazyLines(log: ProcessLogger): LazyList[String]

  /** Starts the process represented by this builder.  The output is returned as
   * a LazyList that blocks when lines are not available but the process has not
   * completed.
   * The producer process will block if the given capacity of lines if filled
   * without being consumed from the LazyList.
   * Standard error is sent to the provided ProcessLogger.  If the
   * process exits with a non-zero value, the LazyList will provide all lines up
   * to termination and then throw an exception.
   */
  def lazyLines(log: ProcessLogger, capacity: Integer): LazyList[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a LazyList that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the console. If the process exits
    * with a non-zero value, the LazyList will provide all lines up to termination
    * but will not throw an exception.
    */
  def lazyLines_! : LazyList[String]

  /** Starts the process represented by this builder.  The output is returned as
   * a LazyList that blocks when lines are not available but the process has not
   * completed.
   * The producer process will block if the given capacity of lines if filled
   * without being consumed from the stream.
   * Standard error is sent to the console. If the process exits
   * with a non-zero value, the LazyList will provide all lines up to termination
   * but will not throw an exception.
   */
  def lazyLines_!(capacity: Integer): LazyList[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a LazyList that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the provided ProcessLogger. If the
    * process exits with a non-zero value, the LazyList will provide all lines up
    * to termination but will not throw an exception.
    */
  def lazyLines_!(log: ProcessLogger): LazyList[String]

  /** Starts the process represented by this builder.  The output is returned as
   * a LazyList that blocks when lines are not available but the process has not
   * completed.
   * The producer process will block if the given capacity of lines if filled
   * without being consumed from the stream.
   * Standard error is sent to the provided ProcessLogger. If the
   * process exits with a non-zero value, the LazyList will provide all lines up
   * to termination but will not throw an exception.
   */
  def lazyLines_!(log: ProcessLogger, capacity: Integer): LazyList[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the console.  If the process exits
    * with a non-zero value, the Stream will provide all lines up to termination
    * and then throw an exception.
    */
  @deprecated("use lazyLines", since = "2.13.0")
  def lineStream: Stream[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.
    * The producer process will block if the given capacity of lines if filled
    * without being consumed from the stream.
    * Standard error is sent to the console.  If the process exits
    * with a non-zero value, the Stream will provide all lines up to termination
    * and then throw an exception.
    */
  @deprecated("use lazyLines", since = "2.13.0")
  def lineStream(capacity: Integer): Stream[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the provided ProcessLogger.  If the
    * process exits with a non-zero value, the Stream will provide all lines up
    * to termination and then throw an exception.
    */
  @deprecated("use lazyLines", since = "2.13.0")
  def lineStream(log: ProcessLogger): Stream[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.
    * The producer process will block if the given capacity of lines if filled
    * without being consumed from the stream.
    * Standard error is sent to the provided ProcessLogger.  If the
    * process exits with a non-zero value, the Stream will provide all lines up
    * to termination and then throw an exception.
    */
  @deprecated("use lazyLines", since = "2.13.0")
  def lineStream(log: ProcessLogger, capacity: Integer): Stream[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the console. If the process exits
    * with a non-zero value, the Stream will provide all lines up to termination
    * but will not throw an exception.
    */
  @deprecated("use lazyLines_!", since = "2.13.0")
  def lineStream_! : Stream[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.
    * The producer process will block if the given capacity of lines if filled
    * without being consumed from the stream.
    * Standard error is sent to the console. If the process exits
    * with a non-zero value, the Stream will provide all lines up to termination
    * but will not throw an exception.
    */
  @deprecated("use lazyLines_!", since = "2.13.0")
  def lineStream_!(capacity: Integer): Stream[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the provided ProcessLogger. If the
    * process exits with a non-zero value, the Stream will provide all lines up
    * to termination but will not throw an exception.
    */
  @deprecated("use lazyLines_!", since = "2.13.0")
  def lineStream_!(log: ProcessLogger): Stream[String]

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.
    * The producer process will block if the given capacity of lines if filled
    * without being consumed from the stream.
    * Standard error is sent to the provided ProcessLogger. If the
    * process exits with a non-zero value, the Stream will provide all lines up
    * to termination but will not throw an exception.
    */
  @deprecated("use lazyLines_!", since = "2.13.0")
  def lineStream_!(log: ProcessLogger, capacity: Integer): Stream[String]

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the exit code.  Standard output and error are sent to the console.
    */
  def runBlocking : Int

  @deprecated("use runBlocking", since = "2.13.0")
  def ! : Int = runBlocking

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the exit code.  Standard output and error are sent to the given
    * ProcessLogger.
    */
  def runBlocking(log: ProcessLogger): Int

  @deprecated("use runBlocking", since = "2.13.0")
  def !(log: ProcessLogger): Int = runBlocking(log)

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the exit code.  Standard output and error are sent to the console.
    * The newly started process reads from standard input of the current process.
    */
  def runBlockingFromStdin : Int

  @deprecated("use runBlockingFromStdin", since = "2.13.0")
  def !< : Int = runBlockingFromStdin

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the exit code.  Standard output and error are sent to the given
    * ProcessLogger.  The newly started process reads from standard input of the
    * current process.
    */
  def runBlockingFromStdin(log: ProcessLogger): Int

  @deprecated("use runBlockingFromStdin", since = "2.13.0")
  def !<(log: ProcessLogger): Int = runBlockingFromStdin(log)

  /** Starts the process represented by this builder.  Standard output and error
   * are sent to the console.*/
  def run(): Process

  /** Starts the process represented by this builder.  Standard output and error
    * are sent to the given ProcessLogger.
    */
  def run(log: ProcessLogger): Process

  /** Starts the process represented by this builder.  I/O is handled by the
    * given ProcessIO instance.
    */
  def run(io: ProcessIO): Process

  /** Starts the process represented by this builder.  Standard output and error
    * are sent to the console.  The newly started process reads from standard
    * input of the current process if `connectInput` is true.
    */
  def run(connectInput: Boolean): Process

  /** Starts the process represented by this builder.  Standard output and error
    * are sent to the given ProcessLogger.  The newly started process reads from
    * standard input of the current process if `connectInput` is true.
    */
  def run(log: ProcessLogger, connectInput: Boolean): Process

  /** Constructs a command that runs this command first and then `other` if this
    * command succeeds.
    */
  def ifSucceedsThen(other: ProcessBuilder): ProcessBuilder

  @deprecated("Use ifSucceedsThen", since = "2.13.0")
  def #&&(other: ProcessBuilder): ProcessBuilder = ifSucceedsThen(other)

  /** Constructs a command that runs this command first and then `other` if this
    * command does not succeed.
    */
  def ifFailsThen(other: ProcessBuilder): ProcessBuilder

  @deprecated("Use ifFailsThen", since = "2.13.0")
  def #||(other: ProcessBuilder): ProcessBuilder = ifFailsThen(other)

  /** Constructs a command that will run this command and pipes the output to
    * `other`.  `other` must be a simple command.
    */
  def pipeTo(other: ProcessBuilder): ProcessBuilder

  @deprecated("Use pipeTo", since = "2.13.0")
  def #|(other: ProcessBuilder): ProcessBuilder = pipeTo(other)

  /** Constructs a command that will run this command and then `other`.  The
    * exit code will be the exit code of `other`.
    */
  def andThen(other: ProcessBuilder): ProcessBuilder

  @deprecated("Use andThen", since = "2.13.0")
  def ###(other: ProcessBuilder): ProcessBuilder = andThen(other)

  /** True if this command can be the target of a pipe.  */
  def canPipeTo: Boolean

  /** True if this command has an exit code which should be propagated to the
    * user.  Given a pipe between A and B, if B.hasExitValue is true then the
    * exit code will be the one from B; if it is false, the one from A.  This
    * exists to prevent output redirections (implemented as pipes) from masking
    * useful process error codes.
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
    def redirect(f: File): ProcessBuilder

    @deprecated("Use redirect", since = "2.13.0")
    def #<<(f: File): ProcessBuilder = redirect(f)

    /** Append the contents from a `java.net.URL` to this file */
    def redirect(u: URL): ProcessBuilder

    @deprecated("Use redirect", since = "2.13.0")
    def #<<(u: URL): ProcessBuilder = redirect(u)

    /** Append the contents of a `java.io.InputStream` to this file */
    def redirect(i: => InputStream): ProcessBuilder

    @deprecated("Use redirect", since = "2.13.0")
    def #<<(i: => InputStream): ProcessBuilder = redirect(i)

    /** Append the contents of a [[scala.sys.process.ProcessBuilder]] to this file */
    def redirect(p: ProcessBuilder): ProcessBuilder

    @deprecated("Use redirect", since = "2.13.0")
    def #<<(p: ProcessBuilder): ProcessBuilder = redirect(p)
  }

  /** Represents everything that can be used as an input to a
    * [[scala.sys.process.ProcessBuilder]].
    */
  trait Source {
    protected def toSource: ProcessBuilder

    /** Writes the output stream of this process to the given file. */
    def overwrite(f: File): ProcessBuilder = toFile(f, append = false)

    @deprecated("Use overwrite", since = "2.13.0")
    def #>(f: File): ProcessBuilder = overwrite(f)

    /** Appends the output stream of this process to the given file. */
    def append(f: File): ProcessBuilder = toFile(f, append = true)

    @deprecated("Use append", since = "2.13.0")
    def #>>(f: File):  ProcessBuilder = append(f)

    /** Writes the output stream of this process to the given OutputStream. The
      * argument is call-by-name, so the stream is recreated, written, and closed each
      * time this process is executed.
      */
    def overwrite(out: => OutputStream): ProcessBuilder = overwrite (new OStreamBuilder(out, "<output stream>"))

    @deprecated("Use overwrite", since = "2.13.0")
    def #>(out: => OutputStream): ProcessBuilder = overwrite(out)

    /** Writes the output stream of this process to a [[scala.sys.process.ProcessBuilder]]. */
    def overwrite(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(toSource, b, false)

    @deprecated("Use overwrite", since = "2.13.0")
    def #>(b: ProcessBuilder): ProcessBuilder = overwrite(b)

    /** Returns a [[scala.sys.process.ProcessBuilder]] representing this `Source`. */
    def cat = toSource
    private def toFile(f: File, append: Boolean) = overwrite (new FileOutput(f, append))
  }

  /** Represents everything that can receive an output from a
    * [[scala.sys.process.ProcessBuilder]].
    */
  trait Sink {
    protected def toSink: ProcessBuilder

    /** Reads the given file into the input stream of this process. */
    def read(f: File): ProcessBuilder = read (new FileInput(f))

    @deprecated("Use read", since = "2.13.0")
    def #<(f: File): ProcessBuilder = read (new FileInput(f))

    /** Reads the given URL into the input stream of this process. */
    def read(f: URL): ProcessBuilder = read (new URLInput(f))

    @deprecated("Use read", since = "2.13.0")
    def #<(f: URL): ProcessBuilder = read (new URLInput(f))

    /** Reads the given InputStream into the input stream of this process. The
      * argument is call-by-name, so the stream is recreated, read, and closed each
      * time this process is executed.
      */
    def read(in: => InputStream): ProcessBuilder = read (new IStreamBuilder(in, "<input stream>"))

    @deprecated("Use read", since = "2.13.0")
    def #<(in: => InputStream): ProcessBuilder = read (new IStreamBuilder(in, "<input stream>"))

    /** Reads the output of a [[scala.sys.process.ProcessBuilder]] into the input stream of this process. */
    def read(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(b, toSink, false)

    @deprecated("Use read", since = "2.13.0")
    def #<(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(b, toSink, false)
  }
}
