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
import java.io.{ FilterInputStream, FilterOutputStream, PipedInputStream, PipedOutputStream }
import java.util.concurrent.LinkedBlockingQueue

/** Runs provided code in a new Thread and returns the Thread instance. */
private object Spawn {
	def apply(f: => Unit): Thread = apply(f, false)
	def apply(f: => Unit, daemon: Boolean): Thread = {
		val thread = new Thread() { override def run() = { f } }
		thread.setDaemon(daemon)
		thread.start()
		thread
	}
}
private object Future {
	def apply[T](f: => T): () => T = {
		val result = new SyncVar[Either[Throwable, T]]
		def run: Unit =
			try result.set(Right(f))
			catch { case e: Exception => result set Left(e) }

		Spawn(run)

		() => result.get match {
			case Right(value)    => value
			case Left(exception) => throw exception
		}
	}
}
object Uncloseable {
	def apply(in: InputStream): InputStream      = new FilterInputStream(in) { override def close() { } }
	def apply(out: OutputStream): OutputStream   = new FilterOutputStream(out) { override def close() { } }
	def protect(in: InputStream): InputStream    = if (in eq System.in) Uncloseable(in) else in
	def protect(out: OutputStream): OutputStream = if ((out eq System.out) || (out eq System.err)) Uncloseable(out) else out
}

private class AndProcess(
  a: ProcessBuilder,
  b: ProcessBuilder,
  io: ProcessIO
) extends SequentialProcess(a, b, io, _ == 0)

private class OrProcess(
  a: ProcessBuilder,
  b: ProcessBuilder,
  io: ProcessIO
) extends SequentialProcess(a, b, io, _ != 0)

private class ProcessSequence(
  a: ProcessBuilder,
  b: ProcessBuilder,
  io: ProcessIO
) extends SequentialProcess(a, b, io, _ => true)

private class SequentialProcess(
  a: ProcessBuilder,
  b: ProcessBuilder,
  io: ProcessIO,
  evaluateSecondProcess: Int => Boolean
) extends CompoundProcess {

	protected[this] override def runAndExitValue() = {
		val first = a.run(io)
		runInterruptible(first.exitValue)(first.destroy()) flatMap { codeA =>
		  if (evaluateSecondProcess(codeA)) {
		    val second = b.run(io)
				runInterruptible(second.exitValue)(second.destroy())
		  }
		  else Some(codeA)
		}
	}
}

private abstract class BasicProcess extends Process {
	def start(): Unit
}

private abstract class CompoundProcess extends BasicProcess {
	def destroy()   = destroyer()
	def exitValue() = getExitValue() getOrElse sys.error("No exit code: process destroyed.")
	def start()     = getExitValue

	protected lazy val (getExitValue, destroyer) = {
		val code = new SyncVar[Option[Int]]()
		code set None
		val thread = Spawn(code.set(runAndExitValue()))

		(
			Future { thread.join(); code.get },
			() => thread.interrupt()
		)
	}

	/** Start and block until the exit value is available and then return it in Some.  Return None if destroyed (use 'run')*/
	protected[this] def runAndExitValue(): Option[Int]

	protected[this] def runInterruptible[T](action: => T)(destroyImpl: => Unit): Option[T] = {
		try Some(action)
		catch { case _: InterruptedException => destroyImpl; None }
	}
}

private class PipedProcesses(a: ProcessBuilder, b: ProcessBuilder, defaultIO: ProcessIO, toError: Boolean) extends CompoundProcess {
	protected[this] override def runAndExitValue() = {
		val currentSource = new SyncVar[Option[InputStream]]
		val pipeOut       = new PipedOutputStream
		val source        = new PipeSource(currentSource, pipeOut, a.toString)
		source.start()

		val pipeIn      = new PipedInputStream(pipeOut)
		val currentSink = new SyncVar[Option[OutputStream]]
		val sink        = new PipeSink(pipeIn, currentSink, b.toString)
		sink.start()

		def handleOutOrError(fromOutput: InputStream) = currentSource put Some(fromOutput)

		val firstIO =
			if (toError)
				defaultIO.withError(handleOutOrError)
			else
				defaultIO.withOutput(handleOutOrError)
		val secondIO = defaultIO.withInput(toInput => currentSink put Some(toInput))

		val second = b.run(secondIO)
		val first = a.run(firstIO)
		try {
			runInterruptible {
				first.exitValue
				currentSource put None
				currentSink put None
				val result = second.exitValue
				result
			} {
				first.destroy()
				second.destroy()
			}
		}
		finally {
			BasicIO.close(pipeIn)
			BasicIO.close(pipeOut)
		}
	}
}
private class PipeSource(currentSource: SyncVar[Option[InputStream]], pipe: PipedOutputStream, label: => String) extends Thread {
	final override def run() {
		currentSource.get match {
			case Some(source) =>
				try BasicIO.transferFully(source, pipe)
				catch { case e: IOException => println("I/O error " + e.getMessage + " for process: " + label); e.printStackTrace() }
				finally {
					BasicIO.close(source)
					currentSource.unset()
				}
				run()
			case None =>
				currentSource.unset()
				BasicIO.close(pipe)
		}
	}
}
private class PipeSink(pipe: PipedInputStream, currentSink: SyncVar[Option[OutputStream]], label: => String) extends Thread {
	final override def run() {
		currentSink.get match {
			case Some(sink) =>
				try BasicIO.transferFully(pipe, sink)
				catch { case e: IOException => println("I/O error " + e.getMessage + " for process: " + label); e.printStackTrace() }
				finally {
					BasicIO.close(sink)
					currentSink.unset()
				}
				run()
			case None =>
				currentSink.unset()
		}
	}
}
/** A thin wrapper around a java.lang.Process.  `ioThreads` are the Threads created to do I/O.
* The implementation of `exitValue` waits until these threads die before returning. */
private class DummyProcess(action: => Int) extends Process {
	private[this] val exitCode = Future(action)
	override def exitValue() = exitCode()
	override def destroy() { }
}
/** A thin wrapper around a java.lang.Process.  `outputThreads` are the Threads created to read from the
* output and error streams of the process.  `inputThread` is the Thread created to write to the input stream of
* the process.
* The implementation of `exitValue` interrupts `inputThread` and then waits until all I/O threads die before
* returning. */
private class SimpleProcess(p: JProcess, inputThread: Thread, outputThreads: List[Thread]) extends Process {
	override def exitValue() = {
		try p.waitFor()                 // wait for the process to terminate
		finally inputThread.interrupt() // we interrupt the input thread to notify it that it can terminate

		outputThreads.foreach(_.join()) // this ensures that all output is complete before returning (waitFor does not ensure this)
		p.exitValue()
	}
	override def destroy() = {
		try p.destroy()
		finally { inputThread.interrupt() }
	}
}
private final class ThreadProcess(thread: Thread, success: SyncVar[Boolean]) extends Process {
	override def exitValue() = {
		thread.join()
		if (success.get) 0 else 1
	}
	override def destroy() { thread.interrupt() }
}

private object Streamed {
	def apply[T](nonzeroException: Boolean): Streamed[T] = {
		val q = new LinkedBlockingQueue[Either[Int, T]]
		def next(): Stream[T] = q.take match {
			case Left(0)    => Stream.empty
			case Left(code) => if (nonzeroException) error("Nonzero exit code: " + code) else Stream.empty
			case Right(s)   => Stream.cons(s, next)
		}
		new Streamed((s: T) => q.put(Right(s)), code => q.put(Left(code)), () => next())
	}
}
private final class Streamed[T](val process: T => Unit, val done: Int => Unit, val stream: () => Stream[T]) extends NotNull