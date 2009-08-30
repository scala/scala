/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.{ BufferedOutputStream, FileOutputStream, PrintStream, File => JFile }
import scala.io.File

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.FakePos //Position
import scala.tools.util.SocketServer

/**
 *  The server part of the fsc offline compiler.  It awaits compilation
 *  commands and executes them.  It caches a compiler instance so
 *  that it can respond more quickly.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
class StandardCompileServer extends SocketServer
{
  def compileSocket: CompileSocket = CompileSocket // todo: make this a lazy val

  val versionMsg = "Fast Scala compiler " +
    Properties.versionString + " -- " +
    Properties.copyrightString

  val MaxCharge = 0.8

  var shutDown: Boolean = false

  private var compiler: Global = null

  private def settingsAreCompatible(s1: Settings, s2: Settings) = s1 == s2

  private def exit(code: Int): Nothing = {
    System.err.close()
    System.out.close()
    Predef.exit(code)
  }

  private val runtime = Runtime.getRuntime()
  import runtime.{ totalMemory, freeMemory, maxMemory }

  var reporter: ConsoleReporter = _

  /** Create a new compiler instance */
  def newGlobal(settings: Settings, reporter: Reporter) =
    new Global(settings, reporter) {
      override def inform(msg: String) = out.println(msg)
    }

  override def timeout() {
    if (!compileSocket.portFile(port).exists)
      fatal("port file no longer exists; skipping cleanup")
  }

  def printMemoryStats() {
    System.out.println("New session, total memory = %s, max memory = %s, free memory = %s".format(
      totalMemory, maxMemory, freeMemory))
    System.out.flush()
  }

  def isMemoryFullEnough() = {
    runtime.gc()
    (totalMemory - freeMemory).toDouble / maxMemory.toDouble > MaxCharge
  }

  protected def newOfflineCompilerCommand(
    arguments: List[String],
    settings: Settings,
    error: String => Unit,
    interactive: Boolean
  ) = new OfflineCompilerCommand(arguments, settings, error, interactive)

  def session() {
    printMemoryStats()
    val password = compileSocket getPassword port
    val guessedPassword = in.readLine()
    val input = in.readLine()
    if ((input ne null) && password == guessedPassword) {
      val args = input.split("\0",-1).toList
      if (args contains "-shutdown") {
        out.println("[Compile server exited]")
        shutDown = true
        return
      }
      if (args contains "-reset") {
        out.println("[Compile server was reset]")
        compiler = null
        return
      }
      def error(msg: String) {
        out.println(/*new Position*/ FakePos("fsc"),
                    msg + "\n  fsc -help  gives more information")
      }
      val command = newOfflineCompilerCommand(args, new Settings(error), error, false)

      reporter = new ConsoleReporter(command.settings, in, out) {
        // disable prompts, so that compile server cannot block
        override def displayPrompt = ()
      }

      if (command.shouldStopWithInfo) {
        reporter.info(null,
                        command.getInfoMessage(newGlobal(command.settings, reporter)), true)
        } else if (command.files.isEmpty)
            reporter.info(null, command.usageMsg, true)
          else {
            try {
              if ((compiler ne null) && settingsAreCompatible(command.settings, compiler.settings)) {
                compiler.settings = command.settings
                compiler.reporter = reporter
              } else {
                if (args contains "-verbose")
                  out.println("[Starting new compile server instance]")
                compiler = newGlobal(command.settings, reporter)
              }
              val c = compiler
              val run = new c.Run()
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
            if (isMemoryFullEnough)
              compiler = null
          }
    }
  }

  /** A directory holding redirected output */
  private val redirectDir = (compileSocket.tmpDir / "output-redirects").createDirectory

  private def redirect(setter: PrintStream => Unit, filename: String): Unit =
    setter(new PrintStream((redirectDir / filename).createFile.bufferedOutput()))

  def main(args: Array[String]) {
    redirect(System.setOut, "scala-compile-server-out.log")
    redirect(System.setErr, "scala-compile-server-err.log")
    System.err.println("...starting server on socket "+port+"...")
    System.err.flush()
    compileSocket.setPort(port)
    run()
    compileSocket.deletePort(port)
    exit(0)
  }
}


object CompileServer extends StandardCompileServer
