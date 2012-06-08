/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.{ BufferedOutputStream, FileOutputStream, PrintStream }
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.reflect.internal.util.FakePos //Position
import scala.tools.util.SocketServer
import settings.FscSettings

/**
 *  The server part of the fsc offline compiler.  It awaits compilation
 *  commands and executes them.  It caches a compiler instance so
 *  that it can respond more quickly.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
class StandardCompileServer extends SocketServer {
  lazy val compileSocket: CompileSocket = CompileSocket

  private var compiler: Global = null
  private def clearCompiler() = compiler = null

  var reporter: ConsoleReporter = _
  var shutdown = false
  var verbose = false

  val versionMsg = "Fast " + Properties.versionMsg

  val MaxCharge = 0.8

  private val runtime = Runtime.getRuntime()
  import runtime.{ totalMemory, freeMemory, maxMemory }

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
    def mb(bytes: Long) = "%dMB".format(bytes / 1000000)
    info("New session: total memory = %s, max memory = %s, free memory = %s".format(
      mb(totalMemory), mb(maxMemory), mb(freeMemory)))
  }

  def isMemoryFullEnough() = {
    runtime.gc()
    (totalMemory - freeMemory).toDouble / maxMemory.toDouble > MaxCharge
  }

  protected def newOfflineCompilerCommand(arguments: List[String], settings: FscSettings): OfflineCompilerCommand =
    new OfflineCompilerCommand(arguments, settings)

  /** Problematically, Settings are only considered equal if every setting
   *  is exactly equal.  In fsc this immediately breaks down because the randomly
   *  chosen temporary outdirs differ between client and server.  Among other
   *  things.  Long term we could use a meaningful equality; short term I'm just
   *  ignoring options which I can see causing a new compiler instance every time
   *  and which do not interestingly influence compilation products.
   */
  def unequalSettings(s1: Settings, s2: Settings): Set[Settings#Setting] = {
    val ignoreSettings = Set("-d", "-encoding", "-currentDir")
    def trim (s: Settings): Set[Settings#Setting] = (
      s.userSetSettings.toSet[Settings#Setting] filterNot (ss => ignoreSettings exists (ss respondsTo _))
    )
    val ss1 = trim(s1)
    val ss2 = trim(s2)

    (ss1 union ss2) -- (ss1 intersect ss2)
  }

  def session() {
    val password        = compileSocket getPassword port
    val guessedPassword = in.readLine()
    val input           = in.readLine()

    def fscError(msg: String): Unit = out println (
      FakePos("fsc") + msg + "\n  fsc -help  gives more information"
    )
    if (input == null || password != guessedPassword)
      return

    val args        = input.split("\0", -1).toList
    val newSettings = new FscSettings(fscError)
    this.verbose    = newSettings.verbose.value
    val command     = newOfflineCompilerCommand(args, newSettings)

    info("Settings after normalizing paths: " + newSettings)
    printMemoryStats()

    // Update the idle timeout if given
    if (!newSettings.idleMins.isDefault) {
      val mins = newSettings.idleMins.value
      if (mins == 0) echo("Disabling idle timeout on compile server.")
      else echo("Setting idle timeout to " + mins + " minutes.")

      this.idleMinutes = mins
    }
    if (newSettings.shutdown.value) {
      shutdown = true
      return out.println("[Compile server exited]")
    }
    if (newSettings.reset.value) {
      clearCompiler()
      out.println("[Compile server was reset]")
      if (command.files.isEmpty)
        return
    }

    reporter = new ConsoleReporter(newSettings, in, out) {
      // disable prompts, so that compile server cannot block
      override def displayPrompt = ()
    }
    def isCompilerReusable: Boolean = {
      if (compiler == null) {
        info("[Creating new instance for compile server.]")
        info("[Compiler version: " + Properties.versionString + ".]")
        return false
      }
      val unequal = unequalSettings(newSettings, compiler.settings)
      if (unequal.nonEmpty) {
        info("[Replacing compiler with new instance because settings are unequal.]")
        info("[Asymmetric settings: " + unequal.mkString(", ") + "]")
      }
      unequal.isEmpty
    }

    if (command.shouldStopWithInfo)
      reporter.echo(command.getInfoMessage(newGlobal(newSettings, reporter)))
    else if (command.files.isEmpty)
      reporter.echo(command.usageMsg)
    else {
      if (isCompilerReusable) {
        info("[Reusing existing Global instance.]")
        compiler.currentSettings = newSettings
        compiler.reporter = reporter
      }
      else {
        compiler = newGlobal(newSettings, reporter)
      }
      val c = compiler
      try new c.Run() compile command.files
      catch {
        case ex @ FatalError(msg) =>
          reporter.error(null, "fatal error: " + msg)
          clearCompiler()
        case ex =>
          warn("Compile server encountered fatal condition: " + ex)
          shutdown = true
          throw ex
      }
    }
    reporter.printSummary()
    if (isMemoryFullEnough) {
      info("Nulling out compiler due to memory utilization.")
      clearCompiler()
    }
  }
}


object CompileServer extends StandardCompileServer {
  /** A directory holding redirected output */
  private lazy val redirectDir = (compileSocket.tmpDir / "output-redirects").createDirectory()

  private def redirect(setter: PrintStream => Unit, filename: String) {
    setter(new PrintStream((redirectDir / filename).createFile().bufferedOutput()))
  }

  def main(args: Array[String]) {
    val debug = args contains "-v"

    if (debug) {
      echo("Starting CompileServer on port " + port)
      echo("Redirect dir is " + redirectDir)
    }

    redirect(System.setOut, "scala-compile-server-out.log")
    redirect(System.setErr, "scala-compile-server-err.log")
    System.err.println("...starting server on socket "+port+"...")
    System.err.flush()
    compileSocket setPort port
    run()

    compileSocket deletePort port
    sys exit 0
  }
}
