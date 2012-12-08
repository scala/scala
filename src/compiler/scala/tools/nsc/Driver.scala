package scala.tools.nsc

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import Properties.{ versionString, copyrightString, residentPromptString }
import scala.reflect.internal.util.{ BatchSourceFile, FakePos }

abstract class Driver {
  
  val prompt = residentPromptString

  val versionMsg = "Scala compiler " +
    versionString + " -- " +
    copyrightString

  var reporter: ConsoleReporter = _
  protected var command: CompilerCommand = _
  protected var settings: Settings = _

  protected def scalacError(msg: String) {
    reporter.error(FakePos("scalac"), msg + "\n  scalac -help  gives more information")
  }

  protected def processSettingsHook(): Boolean = true

  protected def newCompiler(): Global

  protected def doCompile(compiler: Global) {
    if (command.files.isEmpty) {
      reporter.echo(command.usageMsg)
      reporter.echo(compiler.pluginOptionsHelp)
    } else {
      val run = new compiler.Run()
      run compile command.files
      reporter.printSummary()
    }
  }

  def process(args: Array[String]) {
    val ss       = new Settings(scalacError)
    reporter     = new ConsoleReporter(ss)
    command  = new CompilerCommand(args.toList, ss)
    settings = command.settings

    if (settings.version.value) {
      reporter.echo(versionMsg)
    } else if (processSettingsHook()) {
      val compiler = newCompiler()
      try {
        if (reporter.hasErrors)
          reporter.flush()
        else if (command.shouldStopWithInfo)
          reporter.echo(command.getInfoMessage(compiler))
        else
          doCompile(compiler)
      } catch {
        case ex: Throwable =>
          compiler.reportThrowable(ex)
          ex match {
            case FatalError(msg)  => // signals that we should fail compilation.
            case _                => throw ex // unexpected error, tell the outside world.
          }
      }
    }
  }

  def main(args: Array[String]) {
    process(args)
    sys.exit(if (reporter.hasErrors) 1 else 0)
  }

}