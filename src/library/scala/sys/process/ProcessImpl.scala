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
import java.io.{ PipedInputStream, PipedOutputStream }

private[process] trait ProcessImpl {
  self: Process.type =>

  /** Runs provided code in a new Thread and returns the Thread instance. */
  private[process] object Spawn {
    def apply(f: => Unit): Thread = apply(f, false)
    def apply(f: => Unit, daemon: Boolean): Thread = {
      val thread = new Thread() { override def run() = { f } }
      thread.setDaemon(daemon)
      thread.start()
      thread
    }
  }
  private[process] object Future {
    def apply[T](f: => T): () => T = {
      val result = new SyncVar[Either[Throwable, T]]
      def run(): Unit =
        try result set Right(f)
        catch { case e: Exception => result set Left(e) }

      Spawn(run)

      () => result.get match {
        case Right(value)    => value
        case Left(exception) => throw exception
      }
    }
  }

  private[process] class AndProcess(
    a: ProcessBuilder,
    b: ProcessBuilder,
    io: ProcessIO
  ) extends SequentialProcess(a, b, io, _ == 0)

  private[process] class OrProcess(
    a: ProcessBuilder,
    b: ProcessBuilder,
    io: ProcessIO
  ) extends SequentialProcess(a, b, io, _ != 0)

  private[process] class ProcessSequence(
    a: ProcessBuilder,
    b: ProcessBuilder,
    io: ProcessIO
  ) extends SequentialProcess(a, b, io, _ => true)

  private[process] class SequentialProcess(
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

  private[process] abstract class BasicProcess extends Process {
    def start(): Unit
  }

  private[process] abstract class CompoundProcess extends BasicProcess {
    def destroy()   = destroyer()
    def exitValue() = getExitValue() getOrElse scala.sys.error("No exit code: process destroyed.")
    def start()     = getExitValue

    protected lazy val (getExitValue, destroyer) = {
      val code = new SyncVar[Option[Int]]()
      code set None
      val thread = Spawn(code set runAndExitValue())

      (
        Future { thread.join(); code.get },
        () => thread.interrupt()
      )
    }

    /** Start and block until the exit value is available and then return it in Some.  Return None if destroyed (use 'run')*/
    protected[this] def runAndExitValue(): Option[Int]

    protected[this] def runInterruptible[T](action: => T)(destroyImpl: => Unit): Option[T] = {
      try   Some(action)
      catch onInterrupt { destroyImpl; None }
    }
  }

  private[process] class PipedProcesses(a: ProcessBuilder, b: ProcessBuilder, defaultIO: ProcessIO, toError: Boolean) extends CompoundProcess {
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
          val exit1 = first.exitValue
          currentSource put None
          currentSink put None
          val exit2 = second.exitValue
          // Since file redirection (e.g. #>) is implemented as a piped process,
          // we ignore its exit value so cmd #> file doesn't always return 0.
          if (b.hasExitValue) exit2 else exit1
        } {
          first.destroy()
          second.destroy()
        }
      }
      finally {
        BasicIO close pipeIn
        BasicIO close pipeOut
      }
    }
  }

  private[process] abstract class PipeThread(isSink: Boolean, labelFn: () => String) extends Thread {
    def run(): Unit

    private[process] def runloop(src: InputStream, dst: OutputStream): Unit = {
      try     BasicIO.transferFully(src, dst)
      catch   ioFailure(ioHandler)
      finally BasicIO close {
        if (isSink) dst else src
      }
    }
    private def ioHandler(e: IOException) {
      println("I/O error " + e.getMessage + " for process: " + labelFn())
      e.printStackTrace()
    }
  }

  private[process] class PipeSource(
    currentSource: SyncVar[Option[InputStream]],
    pipe: PipedOutputStream,
    label: => String
  ) extends PipeThread(false, () => label) {

    final override def run(): Unit = currentSource.get match {
      case Some(source) =>
        try runloop(source, pipe)
        finally currentSource.unset()

        run()
      case None =>
        currentSource.unset()
        BasicIO close pipe
    }
  }
  private[process] class PipeSink(
    pipe: PipedInputStream,
    currentSink: SyncVar[Option[OutputStream]],
    label: => String
  ) extends PipeThread(true, () => label) {

    final override def run(): Unit = currentSink.get match {
      case Some(sink) =>
        try runloop(pipe, sink)
        finally currentSink.unset()

        run()
      case None =>
        currentSink.unset()
    }
  }

  /** A thin wrapper around a java.lang.Process.  `ioThreads` are the Threads created to do I/O.
  * The implementation of `exitValue` waits until these threads die before returning. */
  private[process] class DummyProcess(action: => Int) extends Process {
    private[this] val exitCode = Future(action)
    override def exitValue() = exitCode()
    override def destroy() { }
  }
  /** A thin wrapper around a java.lang.Process.  `outputThreads` are the Threads created to read from the
  * output and error streams of the process.  `inputThread` is the Thread created to write to the input stream of
  * the process.
  * The implementation of `exitValue` interrupts `inputThread` and then waits until all I/O threads die before
  * returning. */
  private[process] class SimpleProcess(p: JProcess, inputThread: Thread, outputThreads: List[Thread]) extends Process {
    override def exitValue() = {
      try p.waitFor()                   // wait for the process to terminate
      finally inputThread.interrupt()   // we interrupt the input thread to notify it that it can terminate
      outputThreads foreach (_.join())  // this ensures that all output is complete before returning (waitFor does not ensure this)

      p.exitValue()
    }
    override def destroy() = {
      try p.destroy()
      finally inputThread.interrupt()
    }
  }
  private[process] final class ThreadProcess(thread: Thread, success: SyncVar[Boolean]) extends Process {
    override def exitValue() = {
      thread.join()
      if (success.get) 0 else 1
    }
    override def destroy() { thread.interrupt() }
  }
}
