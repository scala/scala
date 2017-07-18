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
import java.io.{ PipedInputStream, PipedOutputStream }

private[process] trait ProcessImpl {
  self: Process.type =>

  /** Runs provided code in a new Thread and returns the Thread instance. */
  private[process] object Spawn {
    def apply(f: => Unit): Thread = apply(f, daemon = false)
    def apply(f: => Unit, daemon: Boolean): Thread = {
      val thread = new Thread() { override def run() = { f } }
      thread.setDaemon(daemon)
      thread.start()
      thread
    }
  }
  private[process] object Future {
    def apply[T](f: => T): (Thread, () => T) = {
      val result = new SyncVar[Either[Throwable, T]]
      def run(): Unit =
        try result.put(Right(f))
        catch { case e: Exception => result.put(Left(e)) }

      val t = Spawn(run())

      (t, () => result.get match {
        case Right(value)    => value
        case Left(exception) => throw exception
      })
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
      runInterruptible(first.exitValue())(first.destroy()) flatMap { codeA =>
        if (evaluateSecondProcess(codeA)) {
          val second = b.run(io)
          runInterruptible(second.exitValue())(second.destroy())
        }
        else Some(codeA)
      }
    }
  }

  private[process] abstract class BasicProcess extends Process {
    def start(): Unit
  }

  private[process] abstract class CompoundProcess extends BasicProcess {
    def isAlive()   = processThread.isAlive()
    def destroy()   = destroyer()
    def exitValue() = futureValue() getOrElse scala.sys.error("No exit code: process destroyed.")
    def start()     = { futureThread ;() }

    protected lazy val (processThread, (futureThread, futureValue), destroyer) = {
      val code = new SyncVar[Option[Int]]()
      val thread = Spawn {
        var value: Option[Int] = None
        try value = runAndExitValue()
        finally code.put(value)
      }

      (
        thread,
        Future(code.get),          // thread.join()
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
    protected[this] override def runAndExitValue() = runAndExitValue(new PipeSource(a.toString), new PipeSink(b.toString))
    protected[this] def runAndExitValue(source: PipeSource, sink: PipeSink): Option[Int] = {
      source connectOut sink
      source.start()
      sink.start()

      /** Release PipeSource, PipeSink and Process in the correct order.
      * If once connect Process with Source or Sink, then the order of releasing them
      * must be Source -> Sink -> Process, otherwise IOException will be thrown. */
      def releaseResources(so: PipeSource, sk: PipeSink, p: Process *) = {
        so.release()
        sk.release()
        p foreach( _.destroy() )
      }

      val firstIO =
        if (toError) defaultIO.withError(source.connectIn)
        else defaultIO.withOutput(source.connectIn)
      val secondIO = defaultIO.withInput(sink.connectOut)

      val second =
        try b.run(secondIO)
        catch onError { err =>
          releaseResources(source, sink)
          throw err
        }
      val first =
        try a.run(firstIO)
        catch onError { err =>
          releaseResources(source, sink, second)
          throw err
        }
      runInterruptible {
        source.join()
        val exit1 = first.exitValue()
        val exit2 = second.exitValue()
        // Since file redirection (e.g. #>) is implemented as a piped process,
        // we ignore its exit value so cmd #> file doesn't always return 0.
        if (b.hasExitValue) exit2 else exit1
      } {
        releaseResources(source, sink, first, second)
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

  private[process] class PipeSource(label: => String) extends PipeThread(false, () => label) {
    protected[this] val pipe = new PipedOutputStream
    protected[this] val source = new LinkedBlockingQueue[Option[InputStream]]
    override def run(): Unit = {
      try {
        source.take match {
          case Some(in) => runloop(in, pipe)
          case None =>
        }
      }
      catch onInterrupt(())
      finally BasicIO close pipe
    }
    def connectIn(in: InputStream): Unit = source add Some(in)
    def connectOut(sink: PipeSink): Unit = sink connectIn pipe
    def release(): Unit = {
      interrupt()
      source add None
      join()
    }
  }
  private[process] class PipeSink(label: => String) extends PipeThread(true, () => label) {
    protected[this] val pipe = new PipedInputStream
    protected[this] val sink = new LinkedBlockingQueue[Option[OutputStream]]
    override def run(): Unit = {
      try {
        sink.take match {
          case Some(out) => runloop(pipe, out)
          case None =>
        }
      }
      catch onInterrupt(())
      finally BasicIO close pipe
    }
    def connectOut(out: OutputStream): Unit = sink add Some(out)
    def connectIn(pipeOut: PipedOutputStream): Unit = pipe connect pipeOut
    def release(): Unit = {
      interrupt()
      sink add None
      join()
    }
  }

  /** A thin wrapper around a java.lang.Process.  `ioThreads` are the Threads created to do I/O.
   *  The implementation of `exitValue` waits until these threads die before returning.
   */
  private[process] class DummyProcess(action: => Int) extends Process {
    private[this] val (thread, value) = Future(action)
    override def isAlive() = thread.isAlive()
    override def exitValue() = value()
    override def destroy() { }
  }

  /** A thin wrapper around a java.lang.Process.  `outputThreads` are the Threads created to read from the
  * output and error streams of the process.  `inputThread` is the Thread created to write to the input stream of
  * the process.
  * The implementation of `exitValue` interrupts `inputThread` and then waits until all I/O threads die before
  * returning. */
  private[process] class SimpleProcess(p: JProcess, inputThread: Thread, outputThreads: List[Thread]) extends Process {
    override def isAlive() = p.isAlive()
    override def exitValue() = {
      try p.waitFor()                   // wait for the process to terminate
      finally inputThread.interrupt()   // we interrupt the input thread to notify it that it can terminate
      outputThreads foreach (_.join())  // this ensures that all output is complete before returning (waitFor does not ensure this)

      p.exitValue()
    }
    override def destroy() = {
      try {
        outputThreads foreach (_.interrupt()) // on destroy, don't bother consuming any more output
        p.destroy()
      }
      finally inputThread.interrupt()
    }
  }
  private[process] final class ThreadProcess(thread: Thread, success: SyncVar[Boolean]) extends Process {
    override def isAlive()   = thread.isAlive()
    override def exitValue() = if (success.get) 0 else 1   // thread.join()
    override def destroy()   = thread.interrupt()
  }
}
