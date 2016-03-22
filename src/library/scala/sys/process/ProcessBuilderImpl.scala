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
import Process._
import java.io.{ FileInputStream, FileOutputStream }
import BasicIO.{ Uncloseable, Streamed }
import Uncloseable.protect

private[process] trait ProcessBuilderImpl {
  self: ProcessBuilder.type =>

  private[process] class DaemonBuilder(underlying: ProcessBuilder) extends AbstractBuilder {
    final def run(io: ProcessIO): Process = underlying.run(io.daemonized())
  }

  private[process] class Dummy(override val toString: String, exitValue: => Int) extends AbstractBuilder {
    override def run(io: ProcessIO): Process = new DummyProcess(exitValue)
    override def canPipeTo = true
  }

  private[process] class URLInput(url: URL) extends IStreamBuilder(url.openStream, url.toString)
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
      val success = new SyncVar[Boolean]
      success put false
      val t = Spawn({
        runImpl(io)
        success.put(true)
      }, io.daemonizeThreads)

      new ThreadProcess(t, success)
    }
  }

  /** Represents a simple command without any redirection or combination. */
  private[process] class Simple(p: JProcessBuilder) extends AbstractBuilder {
    override def run(io: ProcessIO): Process = {
      val process = p.start() // start the external process
      import io._

      // spawn threads that process the input, output, and error streams using the functions defined in `io`
      val inThread  = Spawn(writeInput(process.getOutputStream), daemon = true)
      val outThread = Spawn(processOutput(process.getInputStream), daemonizeThreads)
      val errorThread =
        if (p.redirectErrorStream) Nil
        else List(Spawn(processError(process.getErrorStream), daemonizeThreads))

      new SimpleProcess(process, inThread, outThread :: errorThread)
    }
    override def toString = p.command.toString
    override def canPipeTo = true
  }

  private[scala] abstract class AbstractBuilder extends ProcessBuilder with Sink with Source {
    protected def toSource = this
    protected def toSink = this

    def #|(other: ProcessBuilder): ProcessBuilder  = {
      require(other.canPipeTo, "Piping to multiple processes is not supported.")
      new PipedBuilder(this, other, false)
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

    def lineStream: Stream[String]                       = lineStream(withInput = false, nonZeroException = true, None)
    def lineStream(log: ProcessLogger): Stream[String]   = lineStream(withInput = false, nonZeroException = true, Some(log))
    def lineStream_! : Stream[String]                    = lineStream(withInput = false, nonZeroException = false, None)
    def lineStream_!(log: ProcessLogger): Stream[String] = lineStream(withInput = false, nonZeroException = false, Some(log))

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

    private[this] def lineStream(
      withInput: Boolean,
      nonZeroException: Boolean,
      log: Option[ProcessLogger]
    ): Stream[String] = {
      val streamed = Streamed[String](nonZeroException)
      val process  = run(BasicIO(withInput, streamed.process, log))

      Spawn(streamed done process.exitValue())
      streamed.stream()
    }

    private[this] def runBuffered(log: ProcessLogger, connectInput: Boolean) =
      log buffer run(log, connectInput).exitValue()

    def canPipeTo = false
    def hasExitValue = true
  }

  private[process] class URLImpl(url: URL) extends URLBuilder with Source {
    protected def toSource = new URLInput(url)
  }
  private[process] class FileImpl(base: File) extends FileBuilder with Sink with Source {
    protected def toSource = new FileInput(base)
    protected def toSink   = new FileOutput(base, false)

    def #<<(f: File): ProcessBuilder           = #<<(new FileInput(f))
    def #<<(u: URL): ProcessBuilder            = #<<(new URLInput(u))
    def #<<(s: => InputStream): ProcessBuilder = #<<(new IStreamBuilder(s, "<input stream>"))
    def #<<(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(b, new FileOutput(base, true), false)
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

    override def createProcess(io: ProcessIO) = new PipedProcesses(first, second, io, toError)
  }

  private[process] class AndBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder
  ) extends SequentialBuilder(first, second, "#&&") {
    override def createProcess(io: ProcessIO) = new AndProcess(first, second, io)
  }

  private[process] class OrBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder
  ) extends SequentialBuilder(first, second, "#||") {
    override def createProcess(io: ProcessIO) = new OrProcess(first, second, io)
  }

  private[process] class SequenceBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder
  ) extends SequentialBuilder(first, second, "###") {
    override def createProcess(io: ProcessIO) = new ProcessSequence(first, second, io)
  }
}
