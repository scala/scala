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

package scala
package tools.nsc

import Properties.{versionMsg, residentPromptString}
import scala.reflect.internal.util.FakePos
import scala.tools.nsc.reporters.Reporter
import scala.tools.util.SystemExit

abstract class Driver {

  val prompt = residentPromptString

  var reporter: Reporter = _
  protected var command: CompilerCommand = _
  protected var settings: Settings = _

  /** Forward errors to the (current) reporter. */
  protected def scalacError(msg: String): Unit = {
    reporter.error(FakePos("scalac"), s"$msg\n  scalac -help gives more information")
  }

  /** True to continue compilation. */
  protected def processSettingsHook(): Boolean = {
    if (settings.version.value) { reporter echo versionMsg ; false }
    else !reporter.hasErrors
  }

  protected def newCompiler(): Global

  protected def doCompile(compiler: Global): Unit = {
    if (command.files.isEmpty) {
      reporter.echo(command.usageMsg)
      reporter.echo(compiler.pluginOptionsHelp)
    } else {
      val run = new compiler.Run()
      run compile command.files
      reporter.finish()
    }
  }

  def process(args: Array[String]): Boolean = {
    val ss   = new Settings(scalacError)
    reporter = Reporter(ss)
    command  = new CompilerCommand(args.toList, ss)
    settings = command.settings

    if (processSettingsHook()) {
      val compiler = newCompiler()
      reporter     = compiler.reporter    // adopt the compiler's reporter, which may be custom
      try {
        if (reporter.hasErrors)
          reporter.flush()
        else if (command.shouldStopWithInfo)
          reporter.echo(command.getInfoMessage(compiler))
        else
          doCompile(compiler)
      } catch {
        case _: SystemExit => // user requested to bail
        case ex: Throwable =>
          compiler.reportThrowable(ex)
          ex match {
            case FatalError(msg)  => // signals that we should fail compilation.
            case _                => throw ex // unexpected error, tell the outside world.
          }
      }
    } else if (reporter.hasErrors) reporter.flush()
    !reporter.hasErrors
  }

  def main(args: Array[String]): Unit = System.exit(if (process(args)) 0 else 1)
}
