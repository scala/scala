package scala.tools.nsc

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import Properties.{ versionString, copyrightString }
import scala.tools.nsc.util.{ BatchSourceFile, FakePos }

abstract class Driver {

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
      reporter.info(null, command.usageMsg, true)
      reporter.info(null, compiler.pluginOptionsHelp, true)
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
      reporter.info(null, versionMsg, true)
    } else if (processSettingsHook()) {
      val compiler = newCompiler()
      try {
        if (reporter.hasErrors)
          reporter.flush()
        else if (command.shouldStopWithInfo)
          reporter.info(null, command.getInfoMessage(compiler), true)
        else
          doCompile(compiler)
      } catch {
        case ex =>
          compiler.logThrowable(ex)
          ex match {
            case FatalError(msg)  => reporter.error(null, "fatal error: " + msg)
            case _                => throw ex
          }
      }
    }
  }

  def main(args: Array[String]) {
    process(args)
    sys.exit(if (reporter.hasErrors) 1 else 0)
  }

}