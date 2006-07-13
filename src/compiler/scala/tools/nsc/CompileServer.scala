/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc

import scala.tools.util.SocketServer
import scala.tools.nsc.util.Position
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.doc.DocGenerator
import scala.concurrent.Process.spawn
import java.io._

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object CompileServer extends SocketServer {

  val PRODUCT: String =
    System.getProperty("scala.tool.name", "scalac")
  val VERSION: String =
    System.getProperty("scala.tool.version", "unknown version")
  val COPYRIGHT: String =
    System.getProperty("scala.copyright", "(c) 2002-2006 LAMP/EPFL")
  val versionMsg = PRODUCT + " " + VERSION + " -- " + COPYRIGHT

  val MaxCharge = 0.8

  var shutDown: boolean = false

  private var compiler: Global = null
  private var inSession: boolean = false
  private var progress: boolean = false

  private def settingsAreCompatible(s1: Settings, s2: Settings) =
    s1.encoding.value == s2.encoding.value &&
    s1.classpath.value == s2.classpath.value &&
    s1.sourcepath.value == s2.sourcepath.value &&
    s1.outdir.value == s2.outdir.value &&
    s1.bootclasspath.value == s2.bootclasspath.value &&
    s1.extdirs.value == s2.extdirs.value

  private def exit(code: int): Nothing = {
    System.err.close()
    System.out.close()
    Predef.exit(code)
  }

  private def spawnWatchDog(): unit = spawn {
    while (true) {
      Thread.sleep(10000)
      if (!CompileSocket.portFile(port).exists() && !inSession) {
        progress = false
        spawn {
          Thread.sleep(10000)
          if (!progress) {
            System.err.println("port file no longer exists; exiting")
            exit(1)
          }
        }
      }
    }
  }

  private val runtime = Runtime.getRuntime()

  def session(): unit = {
    System.out.println("New session, total memory = "+runtime.totalMemory()+
                       ", max memory = "+runtime.maxMemory()+
                       ", free memory = "+runtime.freeMemory)
    val password = CompileSocket.getPassword(port)
    val guessedPassword = in.readLine()
    val input = in.readLine()
    if (input != null && password == guessedPassword) {
      try {
        inSession = true
        progress = true
        val args = input.split("\0").toList
        if (args contains "-shutdown") {
          out.println("[Scala compile server exited]")
          shutDown = true
          return
        }
        if (args contains "-reset") {
          out.println("[Scala compile server was reset]")
          compiler = null
        }
        val reporter = new ConsoleReporter(in, out) {
          // disable prompts, so that compile server cannot block
          override def displayPrompt = {}
        }
        def error(msg: String): unit =
          reporter.error(new Position(PRODUCT),
                         msg + "\n  " + PRODUCT + " -help  gives more information")
        val command = new CompilerCommand(args, error, false) {
          override val cmdName = "fsc"
          settings.disable(settings.prompt)
          settings.disable(settings.resident)
          new settings.BooleanSetting("-reset", "Reset compile server caches")
          new settings.BooleanSetting("-shutdown", "Shutdown compile server")
          new settings.StringSetting("-server", "hostname:portnumber",
                                     "Specify compile server socket", "")
          new settings.BooleanSetting("-J<flag>", "Pass <flag> directly to runtime system")
        }

        reporter.prompt = command.settings.prompt.value;
        if (command.settings.version.value)
          reporter.info(null, versionMsg, true)
        else if (command.settings.help.value)
          reporter.info(null, command.usageMsg, true)
        else if (command.files.isEmpty)
          reporter.info(null, command.usageMsg, true)
        else {
          try {
            if (compiler != null && settingsAreCompatible(command.settings, compiler.settings)) {
              compiler.settings = command.settings
              compiler.reporter = reporter
            } else {
              if (args exists ("-verbose" ==))
                out.println("[Starting new Scala compile server instance]")
              compiler = new Global(command.settings, reporter) {
                override def inform(msg: String) = out.println(msg)
              }
            }
            val c = compiler
            val run = new c.Run
            run compile command.files
          } catch {
            case ex @ FatalError(msg) =>
              if (command.settings.debug.value)
                ex.printStackTrace(out);
              reporter.error(null, "fatal error: " + msg)
              compiler = null
            case ex: Throwable =>
              ex.printStackTrace(out);
              reporter.error(null, "fatal error (server aborted): " + ex.getMessage())
              shutDown = true
          }
          reporter.printSummary()
          runtime.gc()
          if ((runtime.totalMemory() - runtime.freeMemory()).toDouble /
              runtime.maxMemory().toDouble > MaxCharge) compiler = null
        }
      } finally {
        inSession = false
      }
    }
  }

  def redirect(setter: PrintStream => unit, filename: String): unit =
    setter(
      new PrintStream(
        new BufferedOutputStream(
          new FileOutputStream(
            new File(System.getProperty("java.io.tmpdir"), filename)))))

  def main(args: Array[String]): unit = {
    redirect(System.setOut, "scala-compile-server-out.log")
    redirect(System.setErr, "scala-compile-server-err.log")
    System.err.println("...starting server on socket "+port+"...")
    System.err.flush()
    spawnWatchDog()
    CompileSocket.setPort(port)
    run()
    CompileSocket.deletePort(port)
    exit(0)
  }
}
