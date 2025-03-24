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

import scala.annotation.unused
import scala.reflect.io.Path
import scala.util.control.NonFatal

class ResidentScriptRunner(settings: GenericRunnerSettings) extends AbstractScriptRunner(settings) with HasCompileSocket {
  lazy val compileSocket = CompileSocket

  /** Compile a script using the fsc compilation daemon.
   */
  protected def doCompile(scriptFile: String) = {
    val scriptPath       = Path(scriptFile).toAbsolute.path
    val compSettingNames = new Settings(msg => throw new RuntimeException(msg)).visibleSettings.toList map (_.name)
    val compSettings     = settings.visibleSettings.toList filter (compSettingNames contains _.name)
    val coreCompArgs     = compSettings flatMap (_.unparse)
    val compArgs         = coreCompArgs ++ List("-Xscript", mainClass, scriptPath)

    // TODO: untangle this mess of top-level objects with their own little view of the mutable world of settings
    compileSocket.verbose = settings.verbose.value

    compileSocket getOrCreateSocket "" match {
      case Some(sock) => compileOnServer(sock, compArgs)
      case _          => false
    }
  }
}

final class DaemonKiller(@unused settings: GenericRunnerSettings) extends ScriptRunner {
  def runScript(script: String, scriptArgs: List[String]) = shutdownDaemon()

  def runScriptText(script: String, scriptArgs: List[String]) = shutdownDaemon()

  private def shutdownDaemon() =
    try {
      new StandardCompileClient().process(Array("-shutdown"))
      None
    } catch {
      case NonFatal(t) => Some(t)
    }
}
