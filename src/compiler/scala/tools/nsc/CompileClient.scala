/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc

import settings.FscSettings
import scala.tools.util.CompileOutputCommon
import scala.sys.SystemProperties.preferIPv4Stack

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
    val extraVmArgs  = if (settings.preferIPv4) List("-D%s=true".format(preferIPv4Stack.key)) else Nil

    val vmArgs  = settings.jvmargs.unparse ++ settings.defines.unparse ++ extraVmArgs
    val fscArgs = args.toList ++ command.extraFscArgs

    if (settings.version) {
      Console println versionMsg
      return true
    }

    info(versionMsg)
    info(args.mkString("[Given arguments: ", " ", "]"))
    info(fscArgs.mkString("[Transformed arguments: ", " ", "]"))
    info(vmArgs.mkString("[VM arguments: ", " ", "]"))

    val socket =
      if (settings.server.value == "") compileSocket.getOrCreateSocket(vmArgs mkString " ", !shutdown, settings.port.value)
      else compileSocket.getSocket(settings.server.value)

    socket match {
      case Some(sock) => compileOnServer(sock, fscArgs)
      case _          =>
        echo(
          if (shutdown) "[No compilation server running.]"
          else "Compilation failed."
        )
        shutdown
    }
  }
}

object CompileClient extends StandardCompileClient {
  def main(args: Array[String]): Unit = sys exit {
    try   { if (process(args)) 0 else 1 }
    catch { case _: Exception => 1 }
  }
}

