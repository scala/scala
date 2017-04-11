/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package sys
package process

import processInternal._
import ProcessBuilder._

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
  * "ls".!
  *
  * // Execute "ls" and assign a `Stream[String]` of its output to "contents".
  * val contents = Process("ls").lineStream
  *
  * // Here we use a `Seq` to make the parameter whitespace-safe
  * def contentsOf(dir: String): String = Seq("ls", dir).!!
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
  *     as input to the second, like Unix pipes. This is achieved with the `#|`
  *     method.
  *   - They can be executed in sequence, with the second starting as soon as
  *     the first ends. This is done by the `###` method.
  *   - The execution of the second one can be conditioned by the return code
  *     (exit status) of the first, either only when it's zero, or only when it's
  *     not zero. The methods `#&&` and `#||` accomplish these tasks.
  *
  * ==Redirecting Input/Output==
  *
  * Though control of input and output can be done when executing the process,
  * there's a few methods that create a new `ProcessBuilder` with a
  * pre-configured input or output. They are `#<`, `#>` and `#>>`, and may take
  * as input either another `ProcessBuilder` (like the pipe described above), or
  * something else such as a `java.io.File` or a `java.io.InputStream`.
  * For example:
  * {{{
  * new URL("http://databinder.net/dispatch/About") #> "grep JSON" #>> new File("About_JSON") !
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
  *   - `!`: blocks until all external commands exit, and returns the exit code
  *     of the last one in the chain of execution.
  *   - `!!`: blocks until all external commands exit, and returns a `String`
  *     with the output generated.
  *   - `lineStream`: returns immediately like `run`, and the output being generated
  *     is provided through a `Stream[String]`. Getting the next element of that
  *     `Stream` may block until it becomes available. This method will throw an
  *     exception if the return code is different than zero -- if this is not
  *     desired, use the `lineStream_!` method.
  *
  * ==Handling Input and Output==
  *
  * If not specified, the input of the external commands executed with `run` or
  * `!` will not be tied to anything, and the output will be redirected to the
  * stdout and stderr of the Scala process. For the methods `!!` and `lineStream`, no
  * input will be provided, and the output will be directed according to the
  * semantics of these methods.
  *
  * Some methods will cause stdin to be used as input. Output can be controlled
  * with a [[scala.sys.process.ProcessLogger]] -- `!!` and `lineStream` will only
  * redirect error output when passed a `ProcessLogger`. If one desires full
  * control over input and output, then a [[scala.sys.process.ProcessIO]] can be
  * used with `run`.
  *
  * For example, we could silence the error output from `lineStream_!` like this:
  * {{{
  * val etcFiles = "find /etc" lineStream_! ProcessLogger(line => ())
  * }}}
  *
  * ==Extended Example==
  *
  * Let's examine in detail one example of usage:
  * {{{
  * import scala.sys.process._
  * "find src -name *.scala -exec grep null {} ;"  #|  "xargs test -z"  #&&  "echo null-free"  #||  "echo null detected"  !
  * }}}
  * Note that every `String` is implicitly converted into a `ProcessBuilder`
  * through the implicits imported from [[scala.sys.process]]. These `ProcessBuilder` are then
  * combined in three different ways.
  *
  *   1. `#|` pipes the output of the first command into the input of the second command. It
  *      mirrors a shell pipe (`|`).
  *   1. `#&&` conditionally executes the second command if the previous one finished with
  *      exit value 0. It mirrors shell's `&&`.
  *   1. `#||` conditionally executes the third command if the exit value of the previous
  *      command is different than zero. It mirrors shell's `||`.
  *
  * Finally, `!` at the end executes the commands, and returns the exit value.
  * Whatever is printed will be sent to the Scala process standard output. If
  * we wanted to capture it, we could run that with `!!` instead.
  *
  * Note: though it is not shown above, the equivalent of a shell's `;` would be
  * `###`. The reason for this name is that `;` is a reserved token in Scala.
  *
  * Note: the `lines` method, though deprecated, may conflict with the `StringLike`
  * method of the same name.  To avoid this, one may wish to call the builders in
  * `Process` instead of importing `scala.sys.process._`.  The example above would be
  * {{{
  * import scala.sys.process.Process
  * Process("find src -name *.scala -exec grep null {} ;") #| Process("xargs test -z") #&& Process("echo null-free") #|| Process("echo null detected") !
  * }}}
  */
trait ProcessBuilder extends Source with Sink {
  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the output as a String.  Standard error is sent to the console.  If
    * the exit code is non-zero, an exception is thrown.
    */
  def !! : String

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the output as a String.  Standard error is sent to the provided
    * ProcessLogger.  If the exit code is non-zero, an exception is thrown.
    */
  def !!(log: ProcessLogger): String

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the output as a String.  Standard error is sent to the console.  If
    * the exit code is non-zero, an exception is thrown.  The newly started
    * process reads from standard input of the current process.
    */
  def !!< : String

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the output as a String.  Standard error is sent to the provided
    * ProcessLogger.  If the exit code is non-zero, an exception is thrown.  The
    * newly started process reads from standard input of the current process.
    */
  def !!<(log: ProcessLogger): String

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the console.  If the process exits
    * with a non-zero value, the Stream will provide all lines up to termination
    * and then throw an exception.
    */
  def lineStream: Stream[String]

  /** Deprecated (renamed). Use `lineStream` instead. */
  @deprecated("use lineStream instead", "2.11.0")
  def lines: Stream[String] = lineStream

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the provided ProcessLogger.  If the
    * process exits with a non-zero value, the Stream will provide all lines up
    * to termination and then throw an exception.
    */
  def lineStream(log: ProcessLogger): Stream[String]

  /** Deprecated (renamed).  Use `lineStream(log: ProcessLogger)` instead. */
  @deprecated("use lineStream instead", "2.11.0")
  def lines(log: ProcessLogger): Stream[String] = lineStream(log)

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the console. If the process exits
    * with a non-zero value, the Stream will provide all lines up to termination
    * but will not throw an exception.
    */
  def lineStream_! : Stream[String]

  /** Deprecated (renamed).  Use `lineStream_!` instead. */
  @deprecated("use lineStream_! instead", "2.11.0")
  def lines_! : Stream[String] = lineStream_!

  /** Starts the process represented by this builder.  The output is returned as
    * a Stream that blocks when lines are not available but the process has not
    * completed.  Standard error is sent to the provided ProcessLogger. If the
    * process exits with a non-zero value, the Stream will provide all lines up
    * to termination but will not throw an exception.
    */
  def lineStream_!(log: ProcessLogger): Stream[String]

  /** Deprecated (renamed).  Use `lineStream_!(log: ProcessLogger)` instead. */
  @deprecated("use lineStream_! instead", "2.11.0")
  def lines_!(log: ProcessLogger): Stream[String] = lineStream_!(log)

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the exit code.  Standard output and error are sent to the console.
    */
  def ! : Int

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the exit code.  Standard output and error are sent to the given
    * ProcessLogger.
    */
  def !(log: ProcessLogger): Int

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the exit code.  Standard output and error are sent to the console.
    * The newly started process reads from standard input of the current process.
    */
  def !< : Int

  /** Starts the process represented by this builder, blocks until it exits, and
    * returns the exit code.  Standard output and error are sent to the given
    * ProcessLogger.  The newly started process reads from standard input of the
    * current process.
    */
  def !<(log: ProcessLogger): Int

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
  def #&& (other: ProcessBuilder): ProcessBuilder

  /** Constructs a command that runs this command first and then `other` if this
    * command does not succeed.
    */
  def #|| (other: ProcessBuilder): ProcessBuilder

  /** Constructs a command that will run this command and pipes the output to
    * `other`.  `other` must be a simple command.
    */
  def #| (other: ProcessBuilder): ProcessBuilder

  /** Constructs a command that will run this command and then `other`.  The
    * exit code will be the exit code of `other`.
    */
  def ### (other: ProcessBuilder): ProcessBuilder


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
    def #> (f: File): ProcessBuilder = toFile(f, append = false)

    /** Appends the output stream of this process to the given file. */
    def #>> (f: File): ProcessBuilder = toFile(f, append = true)

    /** Writes the output stream of this process to the given OutputStream. The
      * argument is call-by-name, so the stream is recreated, written, and closed each
      * time this process is executed.
      */
    def #>(out: => OutputStream): ProcessBuilder = #> (new OStreamBuilder(out, "<output stream>"))

    /** Writes the output stream of this process to a [[scala.sys.process.ProcessBuilder]]. */
    def #>(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(toSource, b, false)

    /** Returns a [[scala.sys.process.ProcessBuilder]] representing this `Source`. */
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
