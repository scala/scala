/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import java.io.PrintWriter

class Colors(enabled: => Boolean) {
  import Console._

  val bold    = colored(BOLD)
  val yellow  = colored(YELLOW)
  val green   = colored(GREEN)
  val blue    = colored(BLUE)
  val red     = colored(RED)
  val red_b   = colored(RED_B)
  val green_b = colored(GREEN_B)
  val cyan    = colored(CYAN)
  val magenta = colored(MAGENTA)

  private def colored(code: String): String => String =
    s => if (enabled) code + s + RESET else s
}

object NestUI {
  private val testNum = new java.util.concurrent.atomic.AtomicInteger(1)
  @volatile private var testNumberFmt = "%3d"
  // @volatile private var testNumber = 1
  private def testNumber = testNumberFmt format testNum.getAndIncrement()
  def resetTestNumber(max: Int = -1) {
    testNum set 1
    val width = if (max > 0) max.toString.length else 3
    testNumberFmt = s"%${width}d"
  }

  var colorEnabled = sys.props contains "partest.colors"
  val color = new Colors(colorEnabled)
  import color._

  val NONE = 0
  val SOME = 1
  val MANY = 2

  private var _outline = ""
  private var _success = ""
  private var _failure = ""
  private var _warning = ""
  private var _default = ""

  private var dotCount = 0
  private val DotWidth = 72

  def leftFlush() {
    if (dotCount != 0) {
      normal("\n")
      dotCount = 0
    }
  }

  def statusLine(state: TestState) = {
    import state._
    import TestState._
    val colorizer = state match {
      case _: Skip     => yellow
      case _: Updated  => cyan
      case s if s.isOk => green
      case _           => red
    }
    val word = bold(colorizer(state.shortStatus))
    f"$word $testNumber - $testIdent%-40s$reasonString"
  }

  def reportTest(state: TestState) = {
    if (isTerse && state.isOk) {
      if (dotCount >= DotWidth) {
        outline("\n.")
        dotCount = 1
      }
      else {
        outline(".")
        dotCount += 1
      }
    }
    else {
      echo(statusLine(state))
      if (!state.isOk && isDiffy) {
        val differ = bold(red("% ")) + "diff "
        state.transcript find (_ startsWith differ) foreach (echo(_))
      }
    }
  }

  def echo(message: String): Unit = synchronized {
    leftFlush()
    print(message + "\n")
  }
  def chatty(msg: String) = if (isVerbose) echo(msg)

  def echoSkipped(msg: String) = echo(yellow(msg))
  def echoPassed(msg: String)  = echo(bold(green(msg)))
  def echoFailed(msg: String)  = echo(bold(red(msg)))
  def echoMixed(msg: String)   = echo(bold(yellow(msg)))
  def echoWarning(msg: String) = echo(bold(red(msg)))

  def initialize(number: Int) = number match {
    case MANY =>
      _outline = Console.BOLD + Console.BLACK
      _success = Console.BOLD + Console.GREEN
      _failure = Console.BOLD +  Console.RED
      _warning = Console.BOLD + Console.YELLOW
      _default = Console.RESET
    case SOME =>
      _outline = Console.BOLD + Console.BLACK
      _success = Console.RESET
      _failure = Console.BOLD + Console.BLACK
      _warning = Console.BOLD + Console.BLACK
      _default = Console.RESET
    case _ =>
  }

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
    println(ConsoleRunnerSpec.programInfo.usage)
    println(ConsoleRunnerSpec.helpMsg)
    sys.exit(1)
  }

  var _verbose = false
  var _debug = false
  var _terse = false
  var _diff  = false

  def isVerbose = _verbose
  def isDebug = _debug
  def isTerse = _terse
  def isDiffy = _diff

  def setVerbose() {
    _verbose = true
  }
  def setDebug() {
    _debug = true
  }
  def setTerse() {
    _terse = true
  }
  def setDiffOnFail() {
    _diff = true
  }
  def verbose(msg: String) {
    if (isVerbose)
      System.err.println(msg)
  }
  def debug(msg: String) {
    if (isDebug)
      System.err.println(msg)
  }
}
