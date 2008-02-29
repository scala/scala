/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant.sabbus

import java.io.File

import scala.tools.nsc._
import scala.tools.nsc.reporters.ConsoleReporter

class ForeignCompiler {

  private var argsBuffer: String = null
  def args: String = argsBuffer
  def args_=(a: String): Unit = {
    if (args != null) throw new Error("Argument must be set only once")
    argsBuffer = a
    nsc
  }

  private val error: (String => Nothing) = { msg => throw new Exception(msg) }

  private def settings = new Settings(error)

  private lazy val reporter = new ConsoleReporter(settings)

  private lazy val nsc: Global = {
    try {
      val command = new CompilerCommand(List.fromString(args, ' '), settings, error, false)
      new Global(command.settings, reporter)
    }
    catch {
      case ex @ FatalError(msg) =>
        throw new Exception(msg, ex)
    }
  }

  def compile(files: Array[File]): Int = {
    val command = new CompilerCommand(files.toList.map(_.toString), settings, error, true)
    (new nsc.Run) compile command.files
    reporter.ERROR.count << 16 | reporter.WARNING.count
  }

}
