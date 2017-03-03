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

trait LoopCommands { self: { def echo(msg: String): Unit } =>
  protected def out: JPrintWriter

  // So outputs can be suppressed.
  def echoCommandMessage(msg: String): Unit = out.println(msg)

  // available commands
  def commands: List[LoopCommand]

  // a single interpreter command
  abstract class LoopCommand(val name: String, val help: String) extends (String => Result) {
    def usage: String = ""
    def usageMsg: String = s":$name${
      if (usage == "") "" else " " + usage
    }"
    def apply(line: String): Result

    // called if no args are given
    def showUsage(): Result = {
      "usage is " + usageMsg
      Result(keepRunning = true, None)
    }

    // subclasses may provide completions
    def completion: Completion = NoCompletion
  }
  object LoopCommand {
    def nullary(name: String, help: String, f: () => Result): LoopCommand =
      new NullaryCmd(name, help, _ => f())

    def cmd(name: String, usage: String, help: String, f: String => Result, completion: Completion = NoCompletion): LoopCommand =
      if (usage == "") new NullaryCmd(name, help, f)
      else new LineCmd(name, usage, help, f, completion)
  }

  /** print a friendly help message */
  def helpCommand(line: String): Result = line match {
    case ""                => helpSummary()
    case CommandMatch(cmd) => echo(f"%n${cmd.help}")
    case _                 => ambiguousError(line)
  }

  def helpSummary() = {
    val usageWidth = commands map (_.usageMsg.length) max
    val formatStr  = s"%-${usageWidth}s %s"

    echo("All commands can be abbreviated, e.g., :he instead of :help.")

    for (cmd <- commands) echo(formatStr.format(cmd.usageMsg, cmd.help))
  }
  def ambiguousError(cmd: String): Result = {
    matchingCommands(cmd) match {
      case Nil  => echo(cmd + ": no such command.  Type :help for help.")
      case xs   => echo(cmd + " is ambiguous: did you mean " + xs.map(":" + _.name).mkString(" or ") + "?")
    }
    Result(keepRunning = true, None)
  }

  // all commands with given prefix
  private def matchingCommands(cmd: String) = commands.filter(_.name.startsWith(cmd.stripPrefix(":")))

  // extract command from partial name, or prefer exact match if multiple matches
  private object CommandMatch {
    def unapply(name: String): Option[LoopCommand] =
      matchingCommands(name) match {
        case Nil      => None
        case x :: Nil => Some(x)
        case xs       => xs find (_.name == name)
      }
  }

  // extract command name and rest of line
  private val commandish = """(\S+)(?:\s+)?(.*)""".r

  def colonCommand(line: String): Result = line.trim match {
    case ""                                  => helpSummary()
    case commandish(CommandMatch(cmd), rest) => cmd(rest)
    case commandish(name, _)                 => ambiguousError(name)
    case _                                   => echo("?")
  }

  import Completion.Candidates

  def colonCompletion(line: String, cursor: Int): Completion = line.trim match {
    case commandish(name @ CommandMatch(cmd), rest) =>
      if (name.length > cmd.name.length) cmd.completion
      else
        new Completion {
          def resetVerbosity(): Unit = ()
          def complete(buffer: String, cursor: Int) = Candidates(cursor - name.length + 1, List(cmd.name))
        }
    case commandish(name, _) if matchingCommands(name).nonEmpty =>
      new Completion {
        def resetVerbosity(): Unit = ()
        def complete(buffer: String, cursor: Int) = Candidates(cursor - name.length + 1, matchingCommands(name).map(_.name))
      }
    case _ => NoCompletion
  }

  class NullaryCmd(name: String, help: String, f: String => Result) extends LoopCommand(name, help) {
    def apply(line: String): Result = f(line)
  }

  class LineCmd(name: String, argWord: String, help: String, f: String => Result, override val completion: Completion) extends LoopCommand(name, help) {
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
