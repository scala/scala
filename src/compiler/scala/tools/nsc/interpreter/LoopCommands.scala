/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

trait LoopCommands {
  protected def out: java.io.PrintWriter

  // a single interpreter command
  sealed abstract class LoopCommand extends (List[String] => Result) {
    def name: String
    def help: String
    def commandError(msg: String) = {
      out.println(":" + name + " " + msg + ".")
      Result(true, None)
    }
    def usage(): String
  }
  case class NoArgs(name: String, help: String, f: () => Result) extends LoopCommand {
    def usage(): String = ":" + name
    def apply(args: List[String]) = if (args.isEmpty) f() else commandError("accepts no arguments")
  }

  case class LineArg(name: String, help: String, f: (String) => Result) extends LoopCommand {
    def usage(): String = ":" + name + " "
    def apply(args: List[String]) = f(args mkString " ")
  }

  case class OneArg(name: String, help: String, f: (String) => Result) extends LoopCommand {
    def usage(): String = ":" + name + " "
    def apply(args: List[String]) =
      if (args.size == 1) f(args.head)
      else commandError("requires exactly one argument")
  }

  case class VarArgs(name: String, help: String, f: (List[String]) => Result) extends LoopCommand {
    def usage(): String = ":" + name + " [arg]"
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
      out println msg
      default
    }
  }
}

