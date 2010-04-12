package scala.tools
package partest
package io

import java.io.{ StringWriter, PrintWriter, Writer }
import scala.tools.nsc.io._
import scala.util.control.ControlThrowable

trait Logging {
  universe: Universe =>

  class PartestANSIWriter extends ANSIWriter(Console.out) {
    override def colorful: Int = ANSIWriter(universe.isAnsi)
    private def printIf(cond: Boolean, msg: String) =
      if (cond) { outline("debug: ") ; println(msg) }

    val verbose = printIf(isVerbose || isDebug, _: String)
    val debug   = printIf(isDebug, _: String)
  }

  lazy val NestUI = new PartestANSIWriter()

  import NestUI.{ _outline, _success, _failure, _warning, _default }

  def markOutline(msg: String) = _outline + msg + _default
  def markSuccess(msg: String) = _success + msg + _default
  def markFailure(msg: String) = _failure + msg + _default
  def markWarning(msg: String) = _warning + msg + _default
  def markNormal(msg: String)  = _default + msg

  def outline(msg: String) = NestUI outline msg
  def success(msg: String) = NestUI success msg
  def failure(msg: String) = NestUI failure msg
  def warning(msg: String) = NestUI warning msg
  def  normal(msg: String) = NestUI normal msg

  def verbose(msg: String) = NestUI verbose msg
  def   debug(msg: String) = NestUI debug msg

  trait EntityLogging {
    self: TestEntity =>

    lazy val logWriter = new LogWriter(logFile)

    /** Redirect stdout and stderr to logFile, run body, return result.
     */
    def loggingOutAndErr[T](body: => T): T = {
      val log = logFile.printStream(append = true)

      val result = try Console.withOut(log) {
        Console.withErr(log) {
          body
        }
      }
      finally log.close()

      // The default cleanup normalizes paths relative to sourcesDir.
      val cleaned = safeLines(logFile) map normalizePaths mkString ("", "\n", "\n")
      logFile writeAll cleaned

      result
    }

    /** XXX needs attention.
     */
    def failureMessage() = safeSlurp(logFile)

    /** For tracing.  Outputs a line describing the next action.  tracePath
     *  is a path wrapper which prints name or full path depending on verbosity.
     */
    def trace(msg: String)    = if (isTrace || isDryRun) System.err.println(">> [%s] %s".format(label, msg))
    def tracePath(path: Path) = if (isVerbose) path.path else path.name

    /** v == verbose.
     */
    def vtrace(msg: String)   = if (isVerbose) trace(msg)

    /** Run body, writes result to logFile.  Any throwable is
     *  caught, stringified, and written to the log.
     */
    def loggingResult(body: => String) =
      try {
        val result = (body split """\r\n|\r|\n""" toList) map (normalizePaths _) mkString ("", "\n", "\n")
        returning(true)(_ => logFile writeAll result)
      } catch {
        case x: ControlThrowable      => throw x
        case x: InterruptedException  => normal(this + " received interrupt, failing.\n") ; false
        case x: Throwable             => logException(x)
      }

    def throwableToString(x: Throwable): String = {
      val w = new StringWriter
      x.printStackTrace(new PrintWriter(w))
      w.toString
    }

    def warnAndLogException(msg: String, ex: Throwable) = {
      val str = msg + throwableToString(ex)
      warning(toStringTrunc(str, 800))
      logWriter append str
    }

    def deleteLog(force: Boolean = false) =
      if (universe.isNoCleanup && !force) debug("Not cleaning up " + logFile)
      else logFile.deleteIfExists()

    def onException(x: Throwable)    { logException(x) }
    def logException(x: Throwable) = {
      val msg = throwableToString(x)
      if (!isTerse)
        normal(msg)

      logWriter append msg
      false
    }
  }

  /** A writer which doesn't create the file until a write comes in.
   */
  class LazilyCreatedWriter(log: File) extends Writer {
    @volatile private var isCreated = false
    private lazy val underlying = {
      isCreated = true
      log.bufferedWriter()
    }

    def flush() = if (isCreated) underlying.flush()
    def close() = if (isCreated) underlying.close()
    def write(chars: Array[Char], off: Int, len: Int) = {
      underlying.write(chars, off, len)
      underlying.flush()
    }
  }

  class LogWriter(log: File) extends PrintWriter(new LazilyCreatedWriter(log), true) {
    override def print(s: String) = {
      super.print(s)
      flush()
    }
  }
}
