/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import collection.{ mutable, immutable }
import mutable.ListBuffer
import language.implicitConversions

class ProcessResult(val line: String) {
  import sys.process._
  private val buffer = new ListBuffer[String]

  val builder  = Process(line)
  val logger   = ProcessLogger(buffer += _)
  val exitCode = builder ! logger
  def lines    = buffer.toList

  def show() = lines foreach println
  override def toString = "`%s` (%d lines, exit %d)".format(line, buffer.size, exitCode)
}
object ProcessResult {
  implicit def processResultToOutputLines(pr: ProcessResult): List[String] = pr.lines
  def apply(line: String): ProcessResult = new ProcessResult(line)
}

trait LoopCommands {
  protected def out: JPrintWriter

  // So outputs can be suppressed.
  def echoCommandMessage(msg: String): Unit = out println msg

  // a single interpreter command
  abstract class LoopCommand(val name: String, val help: String) extends (String => Result) {
    private var _longHelp: String = null
    final def defaultHelp = usageMsg + " (no extended help available.)"
    def hasLongHelp = _longHelp != null || longHelp != defaultHelp
    def withLongHelp(text: String): this.type = { _longHelp = text ; this }
    def longHelp = _longHelp match {
      case null   => defaultHelp
      case text   => text
    }
    def usage: String = ""
    def usageMsg: String = ":" + name + (
      if (usage == "") "" else " " + usage
    )
    def apply(line: String): Result

    // called if no args are given
    def showUsage(): Result = {
      "usage is " + usageMsg
      Result(true, None)
    }

    def onError(msg: String) = {
      out.println("error: " + msg)
      showUsage()
    }
  }
  object LoopCommand {
    def nullary(name: String, help: String, f: () => Result): LoopCommand =
      new NullaryCmd(name, help, _ => f())

    def cmd(name: String, usage: String, help: String, f: String => Result): LoopCommand =
      if (usage == "") new NullaryCmd(name, help, f)
      else new LineCmd(name, usage, help, f)

    def varargs(name: String, usage: String, help: String, f: List[String] => Result): LoopCommand =
      new VarArgsCmd(name, usage, help, f)
  }

  class NullaryCmd(name: String, help: String, f: String => Result) extends LoopCommand(name, help) {
    def apply(line: String): Result = f(line)
  }

  class LineCmd(name: String, argWord: String, help: String, f: String => Result) extends LoopCommand(name, help) {
    override def usage = argWord
    def apply(line: String): Result = f(line)
  }

  class VarArgsCmd(name: String, argWord: String, help: String, f: List[String] => Result)
            extends LoopCommand(name, help) {
    override def usage = argWord
    def apply(line: String): Result = apply(words(line))
    def apply(args: List[String]) = f(args)
  }

  // the result of a single command
  case class Result(val keepRunning: Boolean, val lineToRecord: Option[String])

  object Result {
    // the default result means "keep running, and don't record that line"
    val default = Result(true, None)

    // most commands do not want to micromanage the Result, but they might want
    // to print something to the console, so we accomodate Unit and String returns.
    implicit def resultFromUnit(x: Unit): Result = default
    implicit def resultFromString(msg: String): Result = {
      echoCommandMessage(msg)
      default
    }
  }
}

