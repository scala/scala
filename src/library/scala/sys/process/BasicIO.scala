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
import java.io.{ BufferedReader, InputStreamReader, FilterInputStream, FilterOutputStream }
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.immutable.Stream
import scala.annotation.tailrec

/**
  * This object contains factories for [[scala.sys.process.ProcessIO]],
  * which can be used to control the I/O of a [[scala.sys.process.Process]]
  * when a [[scala.sys.process.ProcessBuilder]] is started with the `run`
  * command.
  *
  * It also contains some helper methods that can be used to in the creation of
  * `ProcessIO`.
  *
  * It is used by other classes in the package in the implementation of various
  * features, but can also be used by client code.
  */
object BasicIO {
  /** Size of the buffer used in all the functions that copy data */
  final val BufferSize = 8192

  /** Used to separate lines in the `processFully` function that takes `Appendable`. */
  final val Newline    = props("line.separator")

  private[process] final class Streamed[T](
    val process:   T => Unit,
    val    done: Int => Unit,
    val  stream:  () => Stream[T]
  )

  private[process] object Streamed {
    def apply[T](nonzeroException: Boolean): Streamed[T] = {
      val q = new LinkedBlockingQueue[Either[Int, T]]
      def next(): Stream[T] = q.take match {
        case Left(0)    => Stream.empty
        case Left(code) => if (nonzeroException) scala.sys.error("Nonzero exit code: " + code) else Stream.empty
        case Right(s)   => Stream.cons(s, next())
      }
      new Streamed((s: T) => q put Right(s), code => q put Left(code), () => next())
    }
  }

  private[process] trait Uncloseable extends Closeable {
    final override def close() { }
  }
  private[process] object Uncloseable {
    def apply(in: InputStream): InputStream      = new FilterInputStream(in) with Uncloseable { }
    def apply(out: OutputStream): OutputStream   = new FilterOutputStream(out) with Uncloseable { }
    def protect(in: InputStream): InputStream    = if (in eq stdin) Uncloseable(in) else in
    def protect(out: OutputStream): OutputStream = if ((out eq stdout) || (out eq stderr)) Uncloseable(out) else out
  }

  /** Creates a `ProcessIO` from a function `String => Unit`. It can attach the
    * process input to stdin, and it will either send the error stream to
    * stderr, or to a `ProcessLogger`.
    *
    * For example, the `ProcessIO` created below will print all normal output
    * while ignoring all error output. No input will be provided.
    * {{{
    * import scala.sys.process.BasicIO
    * val errToDevNull = BasicIO(false, println(_), None)
    * }}}
    *
    * @param withIn True if the process input should be attached to stdin.
    * @param output A function that will be called with the process output.
    * @param log    An optional `ProcessLogger` to which the output should be
    *               sent. If `None`, output will be sent to stderr.
    * @return A `ProcessIO` with the characteristics above.
    */
  def apply(withIn: Boolean, output: String => Unit, log: Option[ProcessLogger]) =
    new ProcessIO(input(withIn), processFully(output), getErr(log))

  /** Creates a `ProcessIO` that appends its output to a `StringBuffer`. It can
    * attach the process input to stdin, and it will either send the error
    * stream to stderr, or to a `ProcessLogger`.
    *
    * For example, the `ProcessIO` created by the function below will store the
    * normal output on the buffer provided, and print all error on stderr. The
    * input will be read from stdin.
    * {{{
    * import scala.sys.process.{BasicIO, ProcessLogger}
    * val printer = ProcessLogger(println(_))
    * def appendToBuffer(b: StringBuffer) = BasicIO(true, b, Some(printer))
    * }}}
    *
    * @param withIn True if the process input should be attached to stdin.
    * @param buffer A `StringBuffer` which will receive the process normal
    *               output.
    * @param log    An optional `ProcessLogger` to which the output should be
    *               sent. If `None`, output will be sent to stderr.
    * @return A `ProcessIO` with the characteristics above.
    */
  def apply(withIn: Boolean, buffer: StringBuffer, log: Option[ProcessLogger]) =
    new ProcessIO(input(withIn), processFully(buffer), getErr(log))

  /** Creates a `ProcessIO` from a `ProcessLogger` . It can attach the
    * process input to stdin.
    *
    * @param withIn True if the process input should be attached to stdin.
    * @param log    A `ProcessLogger` to receive all output, normal and error.
    * @return A `ProcessIO` with the characteristics above.
    */
  def apply(withIn: Boolean, log: ProcessLogger) =
    new ProcessIO(input(withIn), processOutFully(log), processErrFully(log))

  /** Returns a function `InputStream => Unit` given an optional
    * `ProcessLogger`. If no logger is passed, the function will send the output
    * to stderr. This function can be used to create a
    * [[scala.sys.process.ProcessIO]].
    *
    * @param log An optional `ProcessLogger` to which the contents of
    *            the `InputStream` will be sent.
    * @return A function `InputStream => Unit` (used by
    *          [[scala.sys.process.ProcessIO]]) which will send the data to
    *          either the provided `ProcessLogger` or, if `None`, to stderr.
    */
  def getErr(log: Option[ProcessLogger]) = log match {
    case Some(lg) => processErrFully(lg)
    case None     => toStdErr
  }

  private def processErrFully(log: ProcessLogger) = processFully(log err _)
  private def processOutFully(log: ProcessLogger) = processFully(log out _)

  /** Closes a `Closeable` without throwing an exception */
  def close(c: Closeable) = try c.close() catch { case _: IOException => () }

  /** Returns a function `InputStream => Unit` that appends all data read to the
    * provided `Appendable`. This function can be used to create a
    * [[scala.sys.process.ProcessIO]]. The buffer will be appended line by line.
    *
    * @param buffer An `Appendable` such as `StringBuilder` or `StringBuffer`.
    * @return A function `InputStream => Unit` (used by
    *          [[scala.sys.process.ProcessIO]] which will append all data read
    *          from the stream to the buffer.
    */
  def processFully(buffer: Appendable): InputStream => Unit = processFully(appendLine(buffer))

  /** Returns a function `InputStream => Unit` that will call the passed
    * function with all data read. This function can be used to create a
    * [[scala.sys.process.ProcessIO]]. The `processLine` function will be called
    * with each line read, and `Newline` will be appended after each line.
    *
    * @param processLine A function that will be called with all data read from
    *                    the stream.
    * @return A function `InputStream => Unit` (used by
    *          [[scala.sys.process.ProcessIO]] which will call `processLine`
    *          with all data read from the stream.
    */
  def processFully(processLine: String => Unit): InputStream => Unit = in => {
    val reader = new BufferedReader(new InputStreamReader(in))
    try processLinesFully(processLine)(reader.readLine)
    finally reader.close()
  }

  /** Calls `processLine` with the result of `readLine` until the latter returns
   *  `null` or the current thread is interrupted.
   */
  def processLinesFully(processLine: String => Unit)(readLine: () => String) {
    def working = (Thread.currentThread.isInterrupted == false)
    def halting = { Thread.currentThread.interrupt(); null }
    def readFully(): Unit =
      if (working) {
        val line =
          try readLine()
          catch {
            case _: InterruptedException    => halting
            case e: IOException if !working => halting
          }
        if (line != null) {
          processLine(line)
          readFully()
        }
      }
    readFully()
  }

  /** Copy contents of stdin to the `OutputStream`. */
  def connectToIn(o: OutputStream): Unit = transferFully(Uncloseable protect stdin, o)

  /** Returns a function `OutputStream => Unit` that either reads the content
    * from stdin or does nothing. This function can be used by
    * [[scala.sys.process.ProcessIO]].
    */
  def input(connect: Boolean): OutputStream => Unit = { outputToProcess =>
    if (connect) connectToIn(outputToProcess)
    outputToProcess.close()
  }

  /** Returns a `ProcessIO` connected to stdout and stderr, and, optionally, stdin. */
  def standard(connectInput: Boolean): ProcessIO = standard(input(connectInput))

  /** Returns a `ProcessIO` connected to stdout, stderr and the provided `in` */
  def standard(in: OutputStream => Unit): ProcessIO = new ProcessIO(in, toStdOut, toStdErr)

  /** Send all the input from the stream to stderr, and closes the input stream
   * afterwards.
   */
  def toStdErr = (in: InputStream) => transferFully(in, stderr)

  /** Send all the input from the stream to stdout, and closes the input stream
   * afterwards.
   */
  def toStdOut = (in: InputStream) => transferFully(in, stdout)

  /** Copy all input from the input stream to the output stream. Closes the
    * input stream once it's all read.
    */
  def transferFully(in: InputStream, out: OutputStream): Unit =
    try transferFullyImpl(in, out)
    catch onInterrupt(())

  private[this] def appendLine(buffer: Appendable): String => Unit = line => {
    buffer append line
    buffer append Newline
  }

  private[this] def transferFullyImpl(in: InputStream, out: OutputStream) {
    val buffer = new Array[Byte](BufferSize)
    @tailrec def loop() {
      val byteCount = in.read(buffer)
      if (byteCount > 0) {
        out.write(buffer, 0, byteCount)
        // flush() will throw an exception once the process has terminated
        val available = try { out.flush(); true } catch { case _: IOException => false }
        if (available) loop()
      }
    }
    loop()
    in.close()
  }
}
