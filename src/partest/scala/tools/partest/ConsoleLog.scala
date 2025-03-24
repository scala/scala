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

package scala.tools.partest

class ConsoleLog(colorEnabled: Boolean) {

  val bold    = colored(Console.BOLD)
  val yellow  = colored(Console.YELLOW)
  val green   = colored(Console.GREEN)
  val blue    = colored(Console.BLUE)
  val red     = colored(Console.RED)
  val cyan    = colored(Console.CYAN)
  val magenta = colored(Console.MAGENTA)

  private[this] var dotCount = 0
  private[this] final val DotWidth = 72

  def print(text: String) = synchronized {
    Console.out.print(text)
  }

  def leftFlush(): Unit = synchronized {
    if (dotCount != 0) {
      normal("\n")
      dotCount = 0
    }
  }

  private[this] def colored(code: String): String => String =
    s => if (colorEnabled) code + s + Console.RESET else s

  private[this] val (_outline, _success, _failure, _warning, _default) =
    if (colorEnabled) (Console.BOLD, Console.BOLD + Console.GREEN, Console.BOLD + Console.RED, Console.BOLD + Console.YELLOW, Console.RESET)
    else ("", "", "", "", "")

  def outline(msg: String) = print(_outline + msg + _default)

  def success(msg: String) = print(_success  + msg + _default)

  def failure(msg: String) = print(_failure  + msg + _default)

  def warning(msg: String) = print(_warning  + msg + _default)

  def normal(msg: String) = print(_default + msg)

  def echo(message: String): Unit = synchronized {
    leftFlush()
    print(message + "\n")
  }

  def echoSkipped(msg: String) = echo(yellow(msg))
  def echoPassed(msg: String)  = echo(bold(green(msg)))
  def echoFailed(msg: String)  = echo(bold(red(msg)))
  def echoMixed(msg: String)   = echo(bold(yellow(msg)))
  def echoWarning(msg: String) = echo(bold(red(msg)))

  def printDot(): Unit = printProgress(".")
  def printS(): Unit = printProgress(_warning + "s" +_default)
  def printEx(): Unit  = printProgress(_failure + "X" + _default)
  def printUnknown(): Unit  = printProgress(_failure + "?" + _default)  // crash or uninit'd
  private def printProgress(icon: String): Unit = synchronized {
    if (dotCount >= DotWidth) {
      outline("\n" + icon)
      dotCount = 1
    } else {
      outline(icon)
      dotCount += 1
    }
  }
}
