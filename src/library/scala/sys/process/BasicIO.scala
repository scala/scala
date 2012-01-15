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
import java.io.{ BufferedReader, InputStreamReader, FilterInputStream, FilterOutputStream }
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.immutable.Stream

/**
  * This object contains factories for [[scala.sys.process.ProcessIO]],
  * which can be used to control the I/O of a [[scala.sys.process.Process]]
  * when a [[scala.sys.process.ProcessBuilder]] is started with the `run`
  * command.
  */
object BasicIO {
  final val BufferSize = 8192
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
        case Left(code) => if (nonzeroException) sys.error("Nonzero exit code: " + code) else Stream.empty
        case Right(s)   => Stream.cons(s, next)
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

  def apply(withIn: Boolean, output: String => Unit, log: Option[ProcessLogger]) =
    new ProcessIO(input(withIn), processFully(output), getErr(log))

  def apply(withIn: Boolean, buffer: StringBuffer, log: Option[ProcessLogger]) =
    new ProcessIO(input(withIn), processFully(buffer), getErr(log))

  def apply(withIn: Boolean, log: ProcessLogger) =
    new ProcessIO(input(withIn), processOutFully(log), processErrFully(log))

  def getErr(log: Option[ProcessLogger]) = log match {
    case Some(lg) => processErrFully(lg)
    case None     => toStdErr
  }

  private def processErrFully(log: ProcessLogger) = processFully(log err _)
  private def processOutFully(log: ProcessLogger) = processFully(log out _)

  def close(c: Closeable) = try c.close() catch { case _: IOException => () }
  def processFully(buffer: Appendable): InputStream => Unit = processFully(appendLine(buffer))
  def processFully(processLine: String => Unit): InputStream => Unit = in => {
    val reader = new BufferedReader(new InputStreamReader(in))
    processLinesFully(processLine)(reader.readLine)
  }

  def processLinesFully(processLine: String => Unit)(readLine: () => String) {
    def readFully() {
      val line = readLine()
      if (line != null) {
        processLine(line)
        readFully()
      }
    }
    readFully()
  }
  def connectToIn(o: OutputStream): Unit = transferFully(stdin, o)
  def input(connect: Boolean): OutputStream => Unit = if (connect) connectToIn else _ => ()
  def standard(connectInput: Boolean): ProcessIO = standard(input(connectInput))
  def standard(in: OutputStream => Unit): ProcessIO = new ProcessIO(in, toStdOut, toStdErr)

  def toStdErr = (in: InputStream) => transferFully(in, stderr)
  def toStdOut = (in: InputStream) => transferFully(in, stdout)

  def transferFully(in: InputStream, out: OutputStream): Unit =
    try transferFullyImpl(in, out)
    catch onInterrupt(())

  private[this] def appendLine(buffer: Appendable): String => Unit = line => {
    buffer append line
    buffer append Newline
  }

  private[this] def transferFullyImpl(in: InputStream, out: OutputStream) {
    val buffer = new Array[Byte](BufferSize)
    def loop() {
      val byteCount = in.read(buffer)
      if (byteCount > 0) {
        out.write(buffer, 0, byteCount)
        out.flush()
        loop()
      }
    }
    loop()
  }
}
