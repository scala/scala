package scala.tools.nsc.interactive

import scala.concurrent.SyncVar
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{SourceFile, Position, WorkScheduler}
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._

/** Interface of interactive compiler to a client such as an IDE
 */
object REPL /* extends EvalLoop */ {
}

  /** Commands:
   *
   *  reload file1 ... fileN
   *  typeat file line col
   *
   *
  def run() {
    loop { line =>


    }

  def process(args: Array[String]) {
    val settings = new Settings(error)
    reporter = new ConsoleReporter(settings)
    val command = new CompilerCommand(List.fromArray(args), settings, error, false)
    if (command.settings.version.value)
      reporter.info(null, versionMsg, true)
    else {
      if (command.settings.target.value == "msil") {
        val libpath = System.getProperty("msil.libpath")
        if (libpath != null)
          command.settings.assemrefs.value =
            command.settings.assemrefs.value + File.pathSeparator + libpath
      }
      try {
        object compiler extends Global(command.settings, reporter)
        if (reporter.hasErrors) {
          reporter.flush()
          return
        }

        if (command.shouldStopWithInfo) {
          reporter.info(null, command.getInfoMessage(compiler), true)
        } else {
          if (command.settings.resident.value)
            resident(compiler)
          else if (command.files.isEmpty) {
            reporter.info(null, command.usageMsg, true)
            reporter.info(null, compiler.pluginOptionsHelp, true)
          } else {
            val run = new compiler.Run()
            run compile command.files
            reporter.printSummary()
          }
        }
      } catch {
        case ex @ FatalError(msg) =>
          if (true || command.settings.debug.value) // !!!
            ex.printStackTrace();
        reporter.error(null, "fatal error: " + msg)
      }
    }
  }
  val comp = new Global


  def
   */
