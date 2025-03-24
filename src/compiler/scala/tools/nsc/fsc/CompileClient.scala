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

package scala.tools.nsc.fsc

import scala.util.Properties

/** The client part of the fsc offline compiler.  Instead of compiling
 *  things itself, it send requests to a CompileServer.
 */
class StandardCompileClient extends HasCompileSocket with CompileOutputCommon {
  lazy val compileSocket: CompileSocket = CompileSocket

  val versionMsg = "Fast " + Properties.versionMsg
  var verbose    = false

  def process(args: Array[String]): Boolean = {
    // Trying to get out in front of the log messages in case we're
    // going from verbose to not verbose.
    verbose = (args contains "-verbose")

    val settings     = new FscSettings(Console.println)
    val command      = new OfflineCompilerCommand(args.toList, settings)
    val shutdown     = settings.shutdown.value
    val extraVmArgs  = if (settings.preferIPv4.value) List(s"-Djava.net.preferIPv4Stack=true") else Nil

    val vmArgs  = settings.jvmargs.unparse ++ settings.defines.unparse ++ extraVmArgs
    val fscArgs = args.toList ++ command.extraFscArgs

    if (settings.version.value) {
      Console println versionMsg
      return true
    }

    info(versionMsg)
    info(args.mkString("[Given arguments: ", " ", "]"))
    info(fscArgs.mkString("[Transformed arguments: ", " ", "]"))
    info(vmArgs.mkString("[VM arguments: ", " ", "]"))

    val socket = Option(settings.server.value).filter(_.nonEmpty)
                 .map(compileSocket.getSocket)
                 .getOrElse(
      compileSocket.getOrCreateSocket(vmArgs.mkString(" "), !shutdown, settings.port.value)
    )

    socket match {
      case Some(sock)    => compileOnServer(sock, fscArgs)
      case _ if shutdown => echo("[No compilation server running.]") ; true
      case _             => echo("Compilation failed.") ; false
    }
  }
}

object CompileClient extends StandardCompileClient {
  def main(args: Array[String]): Unit = {
    val ok = try process(args) catch { case _: Exception => false }
    System.exit(if (ok) 0 else 1)
  }
}
