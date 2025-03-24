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

package scala.tools.nsc
package fsc

import java.io.PrintStream

import scala.reflect.internal.util.FakePos
import scala.reflect.internal.FatalError
import scala.tools.nsc.io.Directory
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.util.Properties

/**
 *  The server part of the fsc offline compiler.  It awaits compilation
 *  commands and executes them.  It caches a compiler instance so
 *  that it can respond more quickly.
 *
 *  @author Martin Odersky
 */
class StandardCompileServer(fixPort: Int = 0) extends SocketServer(fixPort) {
  lazy val compileSocket: CompileSocket = CompileSocket

  private var compiler: Global = null
  private def clearCompiler() = compiler = null

  var reporter: ConsoleReporter = _
  var shutdown = false
  var verbose = false

  val MaxCharge = 0.8

  private val runtime = Runtime.getRuntime()
  import runtime.{freeMemory, maxMemory, totalMemory}

  override def timeout(): Unit = {
    if (!compileSocket.portFile(port).exists)
      fatal("port file no longer exists; skipping cleanup")
  }

  def printMemoryStats(): Unit = {
    def mb(bytes: Long) = "%10.2fMB".format(bytes / 1048576.0)
    info("New session: total memory = %s, max memory = %s, free memory = %s".format(
      mb(totalMemory), mb(maxMemory), mb(freeMemory)))
  }

  def isMemoryFullEnough() = {
    runtime.gc()
    (totalMemory - freeMemory).toDouble / maxMemory.toDouble > MaxCharge
  }

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
      s.userSetSettings.toSet[Settings#Setting] filterNot (ss => ignoreSettings.exists(ss respondsTo _))
    )
    val ss1 = trim(s1)
    val ss2 = trim(s2)

    (ss1 union ss2) diff (ss1 intersect ss2)
  }

  def session(): Unit = {
    val password        = compileSocket getPassword port
    val guessedPassword = in.readLine()
    val input           = in.readLine()

    def fscError(msg: String): Unit = out println (
      "" + FakePos("fsc") + msg + "\n  fsc -help  gives more information"
    )
    if (input == null || password != guessedPassword)
      return

    val args        = input.split("\u0000", -1).toList
    val newSettings = new FscSettings(fscError)
    val command     = new OfflineCompilerCommand(args, newSettings)
    this.verbose    = newSettings.verbose.value

    info("Settings after normalizing paths: " + newSettings)
    if (!command.files.isEmpty) info("Input files after normalizing paths: " + (command.files mkString ","))
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
      override def displayPrompt() = ()
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
    /* Create a new compiler instance */
    def newGlobal(settings: Settings, reporter: Reporter) = Global(settings, reporter)

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
        case FatalError(msg) =>
          reporter.error(null, "fatal error: " + msg)
          clearCompiler()
        case ex: Throwable =>
          warn("Compile server encountered fatal condition: " + ex)
          reporter.error(null, "Compile server encountered fatal condition: " + ex.getMessage)
          shutdown = true
          throw ex
      }
    }
    reporter.flush()
    reporter.finish()
    if (isMemoryFullEnough()) {
      info("Nulling out compiler due to memory utilization.")
      clearCompiler()
    }
  }
}

object CompileServer {
  /** A directory holding redirected output */
  //private lazy val redirectDir = (compileSocket.tmpDir / "output-redirects").createDirectory()

  private def createRedirect(dir: Directory, filename: String) =
    new PrintStream((dir / filename).createFile().bufferedOutput())

  def main(args: Array[String]) = execute(() => (), args)

  /**
    * The server's main loop.
    *
    * `startupCallback` is used for internal testing; it's called upon server start,
    * notifying the caller that the server is ready to run.
    *
    * WARNING: the callback runs in the server's thread, blocking the server from doing any work
    * until the callback is finished. Callbacks should be kept simple and clients should not try to
    * interact with the server while the callback is processing.
    */
  def execute(startupCallback : () => Unit, args: Array[String]): Unit = {
    val debug = args contains "-v"
    var port = 0

    val i = args.indexOf("-p")
    if (i >= 0 && args.length > i + 1) {
      scala.util.control.Exception.ignoring(classOf[NumberFormatException]) {
        port = args(i + 1).toInt
      }
    }

    // Create instance rather than extend to pass a port parameter.
    val server = new StandardCompileServer(port)
    val redirectDir = server.compileSocket.mkDaemonDir("fsc_redirects")
    if (debug) {
      server.echo("Starting CompileServer on port " + server.port)
      server.echo("Redirect dir is " + redirectDir)
    }

    Console.withErr(createRedirect(redirectDir, "scala-compile-server-err.log")) {
      Console.withOut(createRedirect(redirectDir, "scala-compile-server-out.log")) {
        Console.err.println("...starting server on socket "+server.port+"...")
        Console.err.flush()
        server.compileSocket setPort server.port
        startupCallback()
        server.run()

        server.compileSocket deletePort server.port
      }
    }
  }
}
