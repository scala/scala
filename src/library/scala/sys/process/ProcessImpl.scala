/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.sys.process

import processInternal._

import java.util.concurrent.LinkedBlockingQueue
import java.io.{PipedInputStream, PipedOutputStream}

import scala.annotation.tailrec

private[process] trait ProcessImpl {
  self: Process.type =>

  /** Runs provided code in a new Thread and returns the Thread instance. */
  private[process] object Spawn {
    def apply(prefix: String, daemon: Boolean = false)(f: => Unit): Thread = {
      val thread = new Thread() { override def run() = f }
      thread.setName(prefix + "-spawn-" + thread.getName)
      thread.setDaemon(daemon)
      thread.start()
      thread
    }
  }
  private[process] object Future {
    def apply[T](f: => T): (Thread, () => T) = {
      val result = new LinkedBlockingQueue[Either[Throwable, T]](1)
      def run(): Unit = {
        val value = try Right(f) catch { case e: Exception => Left(e) }
        result.put(value)
      }

      val t = Spawn("Future")(run())

      (t, () => result.take() match {
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
      val code = new LinkedBlockingQueue[Option[Int]](1)
      val thread = Spawn("CompoundProcess") {
        var value: Option[Int] = None
        try value = runAndExitValue()
        catch {
          case _: IndexOutOfBoundsException
             | _: IOException
             | _: NullPointerException
             | _: SecurityException
             | _: UnsupportedOperationException
          => value = Some(-1)
        }
        finally code.put(value)
      }

      (
        thread,
        Future(code.take()),          // thread.join()
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
    protected def newSource: PipeSource = new PipeSource(a.toString)
    protected def newSink:   PipeSink   = new PipeSink(b.toString)
    protected[this] override def runAndExitValue() = runAndExitValue(newSource, newSink)
    protected[this] def runAndExitValue(source: PipeSource, sink: PipeSink): Option[Int] = {
      source connectOut sink
      source.start()
      sink.start()

      /* Release PipeSource, PipeSink and Process in the correct order.
       * If once connect Process with Source or Sink, then the order of releasing them
       * must be Source -> Sink -> Process, otherwise IOException will be thrown.
       */
      def releaseResources(so: PipeSource, sk: PipeSink, ps: Process*) = {
        so.release()
        sk.release()
        ps.foreach(_.destroy())
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
        val exit1 = first.exitValue()
        source.done()
        source.join()
        val exit2 = second.exitValue()
        sink.done()
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
    private def ioHandler(e: IOException): Unit = e.printStackTrace()
  }

  private[process] class PipeSource(label: => String) extends PipeThread(isSink = false, () => label) {
    setName(s"PipeSource($label)-$getName")
    protected[this] val pipe = new PipedOutputStream
    protected[this] val source = new LinkedBlockingQueue[Option[InputStream]](1)
    override final def run(): Unit = {
      @tailrec def go(): Unit =
        source.take() match {
          case Some(in) => runloop(in, pipe) ; go()
          case None =>
        }
      try go()
      catch onInterrupt(())
      finally BasicIO close pipe
    }
    def connectIn(in: InputStream): Unit = source.put(Some(in))
    def connectOut(sink: PipeSink): Unit = sink connectIn pipe
    def release(): Unit = {
      interrupt()
      done()
      join()
    }
    def done() = source.put(None)
  }
  private[process] class PipeSink(label: => String) extends PipeThread(isSink = true, () => label) {
    setName(s"PipeSink($label)-$getName")
    protected[this] val pipe = new PipedInputStream
    protected[this] val sink = new LinkedBlockingQueue[Option[OutputStream]](1)
    override def run(): Unit = {
      @tailrec def go(): Unit =
        sink.take() match {
          case Some(out) => runloop(pipe, out) ; go()
          case None =>
        }
      try go()
      catch onInterrupt(())
      finally BasicIO close pipe
    }
    def connectOut(out: OutputStream): Unit = sink.put(Some(out))
    def connectIn(pipeOut: PipedOutputStream): Unit = pipe connect pipeOut
    def release(): Unit = {
      interrupt()
      done()
      join()
    }
    def done() = sink.put(None)
  }

  /** A thin wrapper around a java.lang.Process.  `ioThreads` are the Threads created to do I/O.
   *  The implementation of `exitValue` waits until these threads die before returning.
   */
  private[process] class DummyProcess(action: => Int) extends Process {
    private[this] val (thread, value) = Future(action)
    override def isAlive() = thread.isAlive()
    override def exitValue() = value()
    override def destroy(): Unit = { }
  }

  /** A thin wrapper around a java.lang.Process.
   *
   *  `outputThreads` are the Threads created to read from the
   *  output and error streams of the process.
   *
   *  `inputThread` is the Thread created to write to the input stream of
   *  the process. It may be null if stdin was inherited.
   *
   *  The implementation of `exitValue` interrupts `inputThread`
   *  and then waits until all I/O threads die before returning.
   */
  private[process] class SimpleProcess(p: JProcess, inputThread: Thread, outputThreads: List[Thread]) extends Process {
    override def isAlive() = p.isAlive()
    override def exitValue() = {
      try p.waitFor()                   // wait for the process to terminate
      finally interrupt()
      outputThreads foreach (_.join())  // this ensures that all output is complete before returning (waitFor does not ensure this)

      p.exitValue()
    }
    override def destroy() = {
      try {
        outputThreads foreach (_.interrupt()) // on destroy, don't bother consuming any more output
        p.destroy()
      }
      finally interrupt()
    }
    // we interrupt the input thread to notify it that it can terminate
    private[this] def interrupt(): Unit = if (inputThread != null) inputThread.interrupt()
  }
  private[process] final class ThreadProcess(thread: Thread, success: LinkedBlockingQueue[Boolean]) extends Process {
    override def isAlive()   = thread.isAlive()
    override def exitValue() = if (success.take()) 0 else 1   // thread.join()
    override def destroy()   = thread.interrupt()
  }
}
