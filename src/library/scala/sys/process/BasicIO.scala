/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.sys
package process

import processAliases._
import java.io.{ BufferedReader, InputStreamReader }

object BasicIO {
	final val BufferSize = 8192
	final val Newline    = props("line.separator")

	def apply(buffer: StringBuffer, log: Option[ProcessLogger], withIn: Boolean) =
	  new ProcessIO(input(withIn), processFully(buffer), getErr(log))
	def apply(log: ProcessLogger, withIn: Boolean) =
	  new ProcessIO(input(withIn), processInfoFully(log), processErrFully(log))

	def getErr(log: Option[ProcessLogger]) = log match {
	  case Some(lg) => processErrFully(lg)
	  case None     => toStdErr
	}

	private def processErrFully(log: ProcessLogger)  = processFully(log error _)
	private def processInfoFully(log: ProcessLogger) = processFully(log info _)

	def ignoreOut = (i: OutputStream) => ()

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
	def connectToIn(o: OutputStream): Unit = transferFully(System.in, o)
	def input(connect: Boolean): OutputStream => Unit = if (connect) connectToIn else ignoreOut
	def standard(connectInput: Boolean): ProcessIO = standard(input(connectInput))
	def standard(in: OutputStream => Unit): ProcessIO = new ProcessIO(in, toStdOut, toStdErr)

	def toStdErr = (in: InputStream) => transferFully(in, System.err)
	def toStdOut = (in: InputStream) => transferFully(in, System.out)

	def transferFully(in: InputStream, out: OutputStream): Unit =
		try transferFullyImpl(in, out)
		catch { case _: InterruptedException => () }

	private[this] def appendLine(buffer: Appendable): String => Unit = line => {
		buffer.append(line)
		buffer.append(Newline)
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

