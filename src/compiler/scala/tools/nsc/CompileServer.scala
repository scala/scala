/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.{ BufferedOutputStream, FileOutputStream, PrintStream }
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.FakePos //Position
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

  val versionMsg = "Fast Scala compiler " +
    Properties.versionString + " -- " +
    Properties.copyrightString

  val MaxCharge = 0.8

  private var compiler: Global = null
  var reporter: ConsoleReporter = _
  var shutdown = false

  private def exit(code: Int): Nothing = {
    System.err.close()
    System.out.close()
    sys.exit(code)
  }

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
    System.out.println("New session, total memory = %s, max memory = %s, free memory = %s".format(
      totalMemory, maxMemory, freeMemory))
    System.out.flush()
  }

  def isMemoryFullEnough() = {
    runtime.gc()
    (totalMemory - freeMemory).toDouble / maxMemory.toDouble > MaxCharge
  }

  protected def newOfflineCompilerCommand(arguments: List[String], settings: Settings) =
    new OfflineCompilerCommand(arguments, settings)

  /** Problematically, Settings are only considered equal if every setting
   *  is exactly equal.  In fsc this immediately breaks down because the randomly
   *  chosen temporary outdirs differ between client and server.  Among other
   *  things.  Long term we could use a meaningful equality; short term I'm just
   *  ignoring options which I can see causing a new compiler instance every time
   *  and which do not interestingly influence compilation products.
   */
  def unequalSettings(s1: Settings, s2: Settings): Set[Settings#Setting] = {
    val ignoreSettings = Set("-d", "-encoding", "-verbose")
    def trim (s: Settings): Set[Settings#Setting] = (
      s.userSetSettings.toSet[Settings#Setting] filterNot (ss => ignoreSettings exists (ss respondsTo _))
    )
    val ss1 = trim(s1)
    val ss2 = trim(s2)

    (ss1 union ss2) -- (ss1 intersect ss2)
  }

  def session() {
    printMemoryStats()
    val password        = compileSocket getPassword port
    val guessedPassword = in.readLine()
    val input           = in.readLine()

    def fscError(msg: String): Unit = out println (
      FakePos("fsc"),
      msg + "\n  fsc -help  gives more information"
    )
    if (input == null || password != guessedPassword)
      return

    val args = input.split("\0", -1).toList
    val settings = new FscSettings(fscError)
    def logVerbose(msg: String) =
      if (settings.verbose.value)
        out println msg

    val command = newOfflineCompilerCommand(args, settings)
    if (settings.shutdown.value) {
      shutdown = true
      return out.println("[Compile server exited]")
    }
    if (settings.reset.value) {
      compiler = null
      return out.println("[Compile server was reset]")
    }

    reporter = new ConsoleReporter(command.settings, in, out) {
      // disable prompts, so that compile server cannot block
      override def displayPrompt = ()
    }
    def isCompilerReusable: Boolean = {
      if (compiler == null) {
        logVerbose("[Creating new instance for compile server.]")
        logVerbose("[Compiler version: " + Properties.versionString + ".]")
        return false
      }
      val unequal = unequalSettings(command.settings, compiler.settings)
      if (unequal.nonEmpty) {
        logVerbose("[Replacing compiler with new instance because settings are unequal.]")
        logVerbose("[Asymmetric settings: " + unequal.mkString(", ") + "]")
      }
      unequal.isEmpty
    }

    if (command.shouldStopWithInfo)
      reporter.info(null, command.getInfoMessage(newGlobal(command.settings, reporter)), true)
    else if (command.files.isEmpty)
      reporter.info(null, command.usageMsg, true)
    else {
      if (isCompilerReusable) {
        compiler.settings = command.settings
        compiler.reporter = reporter
      }
      else {
        compiler = newGlobal(command.settings, reporter)
      }
      val c = compiler
      val run = new c.Run()
      try run compile command.files
      catch {
        case ex @ FatalError(msg) =>
          reporter.error(null, "fatal error: " + msg)
          compiler = null
        case ex =>
          shutdown = true
          throw ex
      }
    }
    reporter.printSummary()
    if (isMemoryFullEnough) {
      logVerbose("Nulling out compiler due to memory utilization.")
      compiler = null
    }
  }
}


object CompileServer extends StandardCompileServer {
  /** A directory holding redirected output */
  private val redirectDir = (compileSocket.tmpDir / "output-redirects").createDirectory()

  private def redirect(setter: PrintStream => Unit, filename: String): Unit =
    setter(new PrintStream((redirectDir / filename).createFile().bufferedOutput()))

  def main(args: Array[String]) {
    redirect(System.setOut, "scala-compile-server-out.log")
    redirect(System.setErr, "scala-compile-server-err.log")
    System.err.println("...starting server on socket "+port+"...")
    System.err.flush()
    compileSocket.setPort(port)
    run()

    compileSocket.deletePort(port)
    sys.exit(0)
  }
}
