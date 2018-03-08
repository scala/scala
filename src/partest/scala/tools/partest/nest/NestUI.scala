/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import java.io.PrintWriter

class Colors(enabled: Boolean) {
  import Console._

  val bold    = colored(BOLD)
  val yellow  = colored(YELLOW)
  val green   = colored(GREEN)
  val blue    = colored(BLUE)
  val red     = colored(RED)
  val cyan    = colored(CYAN)
  val magenta = colored(MAGENTA)

  private def colored(code: String): String => String =
    s => if (enabled) code + s + RESET else s
}

class NestUI(val verbose: Boolean = false, val debug: Boolean = false, val terse: Boolean = false,
             val diffOnFail: Boolean = false, val logOnFail: Boolean = false,
             val colorEnabled: Boolean = sys.props contains "partest.colors") {
  private[this] val testNum = new java.util.concurrent.atomic.AtomicInteger(1)
  @volatile private[this] var testNumberFmt = "%3d"
  private[this] def testNumber = testNumberFmt format testNum.getAndIncrement()
  def resetTestNumber(max: Int = -1) {
    testNum set 1
    val width = if (max > 0) max.toString.length else 3
    testNumberFmt = s"%${width}d"
  }

  val color = new Colors(colorEnabled)
  private val realSysErr = System.err
  import color._

  private[this] val (_outline, _success, _failure, _warning, _default) =
    if (colorEnabled) (Console.BOLD, Console.BOLD + Console.GREEN, Console.BOLD + Console.RED, Console.BOLD + Console.YELLOW, Console.RESET)
    else ("", "", "", "", "")

  private[this] var dotCount = 0
  private[this] val DotWidth = 72

  def leftFlush() {
    if (dotCount != 0) {
      normal("\n")
      dotCount = 0
    }
  }

  def statusLine(state: TestState, durationMs: Long) = {
    import state._
    import TestState._
    val colorizer = state match {
      case _: Skip     => yellow
      case _: Updated  => cyan
      case s if s.isOk => green
      case _           => red
    }
    val word = bold(colorizer(state.shortStatus))
    def durationString = if (durationMs > PartestDefaults.printDurationThreshold) f"[duration ${(1.0 * durationMs) / 1000}%.2fs]" else ""
    f"$word $testNumber - $testIdent%-40s$reasonString$durationString"
  }

  def reportTest(state: TestState, info: TestInfo, durationMs: Long): Unit = {
    if (terse && state.isOk) {
      if (dotCount >= DotWidth) {
        outline("\n.")
        dotCount = 1
      } else {
        outline(".")
        dotCount += 1
      }
    } else {
      echo(statusLine(state, durationMs))
      if (!state.isOk) {
        def showLog() = if (info.logFile.canRead) {
          echo(bold(cyan(s"##### Log file '${info.logFile}' from failed test #####\n")))
          echo(info.logFile.fileContents)
        }
        if (diffOnFail) {
          val differ = bold(red("% ")) + "diff "
          val diffed = state.transcript find (_ startsWith differ)
          diffed match {
            case Some(diff) => echo(diff)
            case None if !logOnFail && !verbose => showLog()
            case _ => ()
          }
        }
        if (logOnFail) showLog()
      }
    }
  }

  def echo(message: String): Unit = synchronized {
    leftFlush()
    print(message + "\n")
  }
  def chatty(msg: String): Unit = if (verbose) echo(msg)

  def echoSkipped(msg: String) = echo(yellow(msg))
  def echoPassed(msg: String)  = echo(bold(green(msg)))
  def echoFailed(msg: String)  = echo(bold(red(msg)))
  def echoMixed(msg: String)   = echo(bold(yellow(msg)))
  def echoWarning(msg: String) = echo(bold(red(msg)))

  def outline(msg: String) = print(_outline + msg + _default)
  def outline(msg: String, wr: PrintWriter) = synchronized {
    wr.print(_outline + msg + _default)
  }

  def success(msg: String) = print(_success  + msg + _default)
  def success(msg: String, wr: PrintWriter) = synchronized {
    wr.print(_success + msg + _default)
  }

  def failure(msg: String) = print(_failure  + msg + _default)
  def failure(msg: String, wr: PrintWriter) = synchronized {
    wr.print(_failure + msg + _default)
  }

  def warning(msg: String) = print(_warning  + msg + _default)

  def normal(msg: String) = print(_default + msg)
  def normal(msg: String, wr: PrintWriter) = synchronized {
    wr.print(_default + msg)
  }

  def usage() {
    println(RunnerSpec.programInfo.usage)
    println(RunnerSpec.helpMsg)
    sys.exit(1)
  }

  def verbose(msg: String): Unit =
    if (verbose) realSysErr.println(msg)

  def debug(msg: String): Unit =
    if (debug) realSysErr.println(msg)

  def showAllJVMInfo(): Unit = {
    vlog(vmArgString)
    vlog(allPropertiesString)
  }

  def vlog(msg: => String) = if (verbose) realSysErr.println(msg)
}
