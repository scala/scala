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

package scala.tools.nsc.interpreter
package shell

import java.io.{PrintWriter => JPrintWriter}
import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.interpreter.ReplStrings.words

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
  protected def echo(msg: String): Unit
  protected def out: JPrintWriter

  // So outputs can be suppressed.
  def echoCommandMessage(msg: String): Unit = out.println(msg)

  // available commands
  def commands: List[LoopCommand]

  // a single interpreter command
  abstract class LoopCommand(
      val name: String,
      val help: String,
      val detailedHelp: Option[String]) extends (String => Result) {
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
    override def toString(): String = name
  }
  object LoopCommand {
    def nullary(name: String, help: String, f: () => Result): LoopCommand =
      nullary(name, help, None, f)

    def nullary(name: String, help: String, detailedHelp: Option[String], f: () => Result): LoopCommand =
      new NullaryCmd(name, help, detailedHelp, _ => f())

    def cmd(name: String, usage: String, help: String, f: String => Result, completion: Completion = NoCompletion): LoopCommand =
      cmdWithHelp(name, usage, help, None, f, completion)

    def cmdWithHelp(name: String, usage: String, help: String, detailedHelp: Option[String],
      f: String => Result, completion: Completion = NoCompletion): LoopCommand =
      if (usage == "") new NullaryCmd(name, help, detailedHelp, f)
      else new LineCmd(name, usage, help, detailedHelp, f, completion)
  }

  /** print a friendly help message */
  def helpCommand(line: String): Result = line match {
    case ""                => helpSummary()
    case CommandMatch(cmd) => echo(f"%n${cmd.detailedHelp getOrElse cmd.help}")
    case _                 => ambiguousError(line)
  }

  def helpSummary() = {
    val usageWidth = commands.map(_.usageMsg.length).max
    val formatStr  = s"%-${usageWidth}s %s"

    echo("All commands can be abbreviated, e.g., :he instead of :help.")

    for (cmd <- commands) echo(formatStr.format(cmd.usageMsg, cmd.help))
    echo("")
    echo("Useful default key bindings:")
    echo("  TAB           code completion")
    echo("  CTRL-ALT-T    show type at cursor, hit again to show code with types/implicits inferred.")
  }
  def ambiguousError(cmd: String): Result = {
    matchingCommands(cmd) match {
      case Nil  => echo(s"No such command '$cmd'.  Type :help for help.")
      case xs   => echo(cmd + " is ambiguous: did you mean " + xs.map(":" + _.name).mkString(" or ") + "?")
    }
    Result(keepRunning = true, None)
  }

  // all commands with given prefix
  private def matchingCommands(cmd: String) = commands.filter(_.name.startsWith(cmd.stripPrefix(":")))

  // extract unique command from partial name, or prefer exact match if multiple matches
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

  // expect line includes leading colon
  def colonCommand(line: String): Result = line.trim match {
    case ""                                  => helpSummary()
    case commandish(CommandMatch(cmd), rest) => cmd(rest)
    case commandish(name, _)                 => ambiguousError(name)
    case _                                   => echo("?")
  }

  def colonCompletion(line: String, cursor: Int): Completion =
    line match {
      case commandish(name0, rest) =>
        val name = name0 take cursor
        val cmds = matchingCommands(name)
        val cursorAtName = cursor <= name.length
        cmds match {
          case Nil                            => NoCompletion
          case cmd :: Nil if !cursorAtName    => cmd.completion
          case cmd :: Nil if cmd.name == name => NoCompletion
          case cmd :: Nil =>
            val completion = ":" + cmd.name
            new Completion {
              def complete(buffer: String, cursor: Int, filter: Boolean) =
                CompletionResult(buffer, cursor = 1, List(CompletionCandidate(completion)), "", "")
            }
          case cmd :: _ =>
            new Completion {
              def complete(buffer: String, cursor: Int, filter: Boolean) =
                CompletionResult(buffer, cursor = 1, cmds.map(cmd => CompletionCandidate(":" + cmd.name)), "", "")
            }
        }
      case _ => NoCompletion
    }

  class NullaryCmd(name: String, help: String, detailedHelp: Option[String],
    f: String => Result) extends LoopCommand(name, help, detailedHelp) {
    def apply(line: String): Result = f(line)
  }

  class LineCmd(name: String, argWord: String, help: String, detailedHelp: Option[String],
    f: String => Result, override val completion: Completion) extends LoopCommand(name, help, detailedHelp) {
    override def usage = argWord
    def apply(line: String): Result = f(line)
  }

  class VarArgsCmd(name: String, argWord: String, help: String, detailedHelp: Option[String],
      f: List[String] => Result) extends LoopCommand(name, help, detailedHelp) {
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
