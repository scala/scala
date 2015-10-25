/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package tools
package nsc
package interpreter

import scala.language.implicitConversions

import scala.collection.mutable.ListBuffer

class ProcessResult(val line: String) {
  import scala.sys.process._
  private val buffer = new ListBuffer[String]

  val builder  = Process(line)
  val logger   = ProcessLogger(buffer += _)
  val exitCode = builder ! logger
  def lines    = buffer.toList

  override def toString = "`%s` (%d lines, exit %d)".format(line, buffer.size, exitCode)
}

trait LoopCommands {
  protected def out: JPrintWriter

  // So outputs can be suppressed.
  def echoCommandMessage(msg: String): Unit = out println msg

  // a single interpreter command
  abstract class LoopCommand(val name: String, val help: String) extends (String => Result) {
    def usage: String = ""
    def usageMsg: String = ":" + name + (
      if (usage == "") "" else " " + usage
    )
    def apply(line: String): Result

    // called if no args are given
    def showUsage(): Result = {
      "usage is " + usageMsg
      Result(keepRunning = true, None)
    }
  }
  object LoopCommand {
    def nullary(name: String, help: String, f: () => Result): LoopCommand =
      new NullaryCmd(name, help, _ => f())

    def cmd(name: String, usage: String, help: String, f: String => Result): LoopCommand =
      if (usage == "") new NullaryCmd(name, help, f)
      else new LineCmd(name, usage, help, f)
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
  case class Result(keepRunning: Boolean, lineToRecord: Option[String])

  object Result {
    // the default result means "keep running, and don't record that line"
    val default = Result(keepRunning = true, None)

    // "keep running, and record this line"
    def recording(line: String) = Result(keepRunning = true, Option(line))

    // most commands do not want to micromanage the Result, but they might want
    // to print something to the console, so we accommodate Unit and String returns.
    implicit def resultFromUnit(x: Unit): Result = default
    implicit def resultFromString(msg: String): Result = {
      echoCommandMessage(msg)
      default
    }
  }
}
