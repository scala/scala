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

package scala
package sys
package process

import processInternal._
import Process._
import BasicIO.{LazilyListed, Streamed, Uncloseable}
import Uncloseable.protect

import java.io.{FileInputStream, FileOutputStream}
import java.util.concurrent.LinkedBlockingQueue

import scala.util.control.NonFatal

private[process] trait ProcessBuilderImpl {
  self: ProcessBuilder.type =>

  private[process] class DaemonBuilder(underlying: ProcessBuilder) extends AbstractBuilder {
    final def run(io: ProcessIO): Process = underlying.run(io.daemonized())
  }

  private[process] class Dummy(override val toString: String, exitValue: => Int) extends AbstractBuilder {
    override def run(io: ProcessIO): Process = new DummyProcess(exitValue)
    override def canPipeTo = true
  }

  private[process] class URLInput(url: URL) extends IStreamBuilder(url.openStream(), url.toString)
  private[process] class FileInput(file: File) extends IStreamBuilder(new FileInputStream(file), file.getAbsolutePath)
  private[process] class FileOutput(file: File, append: Boolean) extends OStreamBuilder(new FileOutputStream(file, append), file.getAbsolutePath)

  private[process] class OStreamBuilder(
    stream: => OutputStream,
    label: String
  ) extends ThreadBuilder(label, _ writeInput protect(stream)) {
    override def hasExitValue = false
  }

  private[process] class IStreamBuilder(
    stream: => InputStream,
    label: String
  ) extends ThreadBuilder(label, _ processOutput protect(stream)) {
    override def hasExitValue = false
  }

  private[process] abstract class ThreadBuilder(
    override val toString: String,
    runImpl: ProcessIO => Unit
  ) extends AbstractBuilder {

    override def run(io: ProcessIO): Process = {
      val success = new LinkedBlockingQueue[Boolean](1)
      def go(): Unit = {
        var ok = false
        try {
          runImpl(io)
          ok = true
        } finally success.put(ok)
      }
      val t = Spawn("ThreadProcess", io.daemonizeThreads)(go())
      new ThreadProcess(t, success)
    }
  }

  /** Represents a simple command without any redirection or combination. */
  private[process] class Simple(p: JProcessBuilder) extends AbstractBuilder {
    override def run(io: ProcessIO): Process = {
      import java.lang.ProcessBuilder.Redirect.{INHERIT => Inherit}
      import io.{daemonizeThreads, processError, processOutput, writeInput}

      val inherit = writeInput eq BasicIO.connectToStdIn
      if (inherit) p.redirectInput(Inherit)

      val process = p.start() // start the external process

      // spawn threads that process the input, output, and error streams using the functions defined in `io`
      val inThread =
        if (inherit || (writeInput eq BasicIO.connectNoOp)) null
        else Spawn("Simple-input", daemon = true)(writeInput(process.getOutputStream))
      val outThread = Spawn("Simple-output", daemonizeThreads)(processOutput(process.getInputStream()))
      val errorThread =
        if (p.redirectErrorStream) Nil
        else List(Spawn("Simple-error", daemonizeThreads)(processError(process.getErrorStream())))

      new SimpleProcess(process, inThread, outThread :: errorThread)
    }
    override def toString = p.command.toString
    override def canPipeTo = true
  }

  private[scala] abstract class AbstractBuilder extends ProcessBuilder with Sink with Source {
    protected def toSource: AbstractBuilder = this
    protected def toSink: AbstractBuilder = this

    private[this] val defaultStreamCapacity = 4096

    def #|(other: ProcessBuilder): ProcessBuilder  = {
      require(other.canPipeTo, "Piping to multiple processes is not supported.")
      new PipedBuilder(this, other, toError = false)
    }
    def #||(other: ProcessBuilder): ProcessBuilder = new OrBuilder(this, other)
    def #&&(other: ProcessBuilder): ProcessBuilder = new AndBuilder(this, other)
    def ###(other: ProcessBuilder): ProcessBuilder = new SequenceBuilder(this, other)

    def run(): Process                                          = run(connectInput = false)
    def run(connectInput: Boolean): Process                     = run(BasicIO.standard(connectInput))
    def run(log: ProcessLogger): Process                        = run(log, connectInput = false)
    def run(log: ProcessLogger, connectInput: Boolean): Process = run(BasicIO(connectInput, log))

    def !!                      = slurp(None, withIn = false)
    def !!(log: ProcessLogger)  = slurp(Some(log), withIn = false)
    def !!<                     = slurp(None, withIn = true)
    def !!<(log: ProcessLogger) = slurp(Some(log), withIn = true)

    def lazyLines: LazyList[String]                       = lazyLines(withInput = false, nonZeroException = true, None, defaultStreamCapacity)
    def lazyLines(log: ProcessLogger): LazyList[String]   = lazyLines(withInput = false, nonZeroException = true, Some(log), defaultStreamCapacity)
    def lazyLines_! : LazyList[String]                    = lazyLines(withInput = false, nonZeroException = false, None, defaultStreamCapacity)
    def lazyLines_!(log: ProcessLogger): LazyList[String] = lazyLines(withInput = false, nonZeroException = false, Some(log), defaultStreamCapacity)
    def lazyLines(capacity: Integer): LazyList[String]                       = lazyLines(withInput = false, nonZeroException = true, None, capacity)
    def lazyLines(log: ProcessLogger, capacity: Integer): LazyList[String]   = lazyLines(withInput = false, nonZeroException = true, Some(log), capacity)
    def lazyLines_!(capacity: Integer) : LazyList[String]                    = lazyLines(withInput = false, nonZeroException = false, None, capacity)
    def lazyLines_!(log: ProcessLogger, capacity: Integer): LazyList[String] = lazyLines(withInput = false, nonZeroException = false, Some(log), capacity)

    @deprecated("internal", since = "2.13.4") def lineStream: Stream[String]                       = lineStream(withInput = false, nonZeroException = true, None, defaultStreamCapacity)
    @deprecated("internal", since = "2.13.4") def lineStream(log: ProcessLogger): Stream[String]   = lineStream(withInput = false, nonZeroException = true, Some(log), defaultStreamCapacity)
    @deprecated("internal", since = "2.13.4") def lineStream_! : Stream[String]                    = lineStream(withInput = false, nonZeroException = false, None, defaultStreamCapacity)
    @deprecated("internal", since = "2.13.4") def lineStream_!(log: ProcessLogger): Stream[String] = lineStream(withInput = false, nonZeroException = false, Some(log), defaultStreamCapacity)
    @deprecated("internal", since = "2.13.4") def lineStream(capacity: Integer): Stream[String]                       = lineStream(withInput = false, nonZeroException = true, None, capacity)
    @deprecated("internal", since = "2.13.4") def lineStream(log: ProcessLogger, capacity: Integer): Stream[String]   = lineStream(withInput = false, nonZeroException = true, Some(log), capacity)
    @deprecated("internal", since = "2.13.4") def lineStream_!(capacity: Integer) : Stream[String]                    = lineStream(withInput = false, nonZeroException = false, None, capacity)
    @deprecated("internal", since = "2.13.4") def lineStream_!(log: ProcessLogger, capacity: Integer): Stream[String] = lineStream(withInput = false, nonZeroException = false, Some(log), capacity)

    def !                      = run(connectInput = false).exitValue()
    def !(io: ProcessIO)       = run(io).exitValue()
    def !(log: ProcessLogger)  = runBuffered(log, connectInput = false)
    def !<                     = run(connectInput = true).exitValue()
    def !<(log: ProcessLogger) = runBuffered(log, connectInput = true)

    /** Constructs a new builder which runs this command with all input/output threads marked
     *  as daemon threads.  This allows the creation of a long running process while still
     *  allowing the JVM to exit normally.
     *
     *  Note: not in the public API because it's not fully baked, but I need the capability
     *  for fsc.
     */
    def daemonized(): ProcessBuilder = new DaemonBuilder(this)

    private[this] def slurp(log: Option[ProcessLogger], withIn: Boolean): String = {
      val buffer = new StringBuffer
      val code   = this ! BasicIO(withIn, buffer, log)

      if (code == 0) buffer.toString
      else scala.sys.error("Nonzero exit value: " + code)
    }

    private[this] def lazyLines(
      withInput: Boolean,
      nonZeroException: Boolean,
      log: Option[ProcessLogger],
      capacity: Integer
    ): LazyList[String] = {
      val lazilyListed = LazilyListed[String](nonZeroException, capacity)
      val process      = run(BasicIO(withInput, lazilyListed.process, log))

      // extract done from lazilyListed so that the anonymous function below closes over just the done and not the whole lazilyListed (see https://github.com/scala/bug/issues/12185)
      val done = lazilyListed.done

      Spawn("LazyLines") {
        done {
          try process.exitValue()
          catch {
            case NonFatal(_) => -2
          }
        }
      }
      lazilyListed.lazyList
    }

    @deprecated("internal", since = "2.13.4")
    private[this] def lineStream(
      withInput: Boolean,
      nonZeroException: Boolean,
      log: Option[ProcessLogger],
      capacity: Integer
    ): Stream[String] = {
      val streamed = Streamed[String](nonZeroException, capacity)
      val process  = run(BasicIO(withInput, streamed.process, log))

      Spawn("LineStream")(streamed done process.exitValue())
      streamed.stream()
    }

    private[this] def runBuffered(log: ProcessLogger, connectInput: Boolean) =
      log buffer run(log, connectInput).exitValue()

    def canPipeTo = false
    def hasExitValue = true
  }

  private[process] class URLImpl(url: URL) extends URLBuilder with Source {
    protected def toSource: URLInput = new URLInput(url)
  }
  private[process] class FileImpl(base: File) extends FileBuilder with Sink with Source {
    protected def toSource: FileInput = new FileInput(base)
    protected def toSink: FileOutput = new FileOutput(base, append = false)

    def #<<(f: File): ProcessBuilder           = #<<(new FileInput(f))
    def #<<(u: URL): ProcessBuilder            = #<<(new URLInput(u))
    def #<<(s: => InputStream): ProcessBuilder = #<<(new IStreamBuilder(s, "<input stream>"))
    def #<<(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(b, new FileOutput(base, append = true), toError = false)
  }

  private[process] abstract class BasicBuilder extends AbstractBuilder {
    protected[this] def checkNotThis(a: ProcessBuilder) = require(a != this, "Compound process '" + a + "' cannot contain itself.")
    final def run(io: ProcessIO): Process = {
      val p = createProcess(io)
      p.start()
      p
    }
    protected[this] def createProcess(io: ProcessIO): BasicProcess
  }

  private[process] abstract class SequentialBuilder(
    a: ProcessBuilder,
    b: ProcessBuilder,
    operatorString: String
  ) extends BasicBuilder {

    checkNotThis(a)
    checkNotThis(b)
    override def toString = " ( " + a + " " + operatorString + " " + b + " ) "
  }

  private[process] class PipedBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder,
    toError: Boolean
  ) extends SequentialBuilder(first, second, if (toError) "#|!" else "#|") {

    override def createProcess(io: ProcessIO): PipedProcesses = new PipedProcesses(first, second, io, toError)
  }

  private[process] class AndBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder
  ) extends SequentialBuilder(first, second, "#&&") {
    override def createProcess(io: ProcessIO): AndProcess = new AndProcess(first, second, io)
  }

  private[process] class OrBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder
  ) extends SequentialBuilder(first, second, "#||") {
    override def createProcess(io: ProcessIO): OrProcess = new OrProcess(first, second, io)
  }

  private[process] class SequenceBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder
  ) extends SequentialBuilder(first, second, "###") {
    override def createProcess(io: ProcessIO): ProcessSequence = new ProcessSequence(first, second, io)
  }
}
