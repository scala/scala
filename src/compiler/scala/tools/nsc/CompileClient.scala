/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.{ BufferedReader, File, InputStreamReader, PrintWriter }
import Properties.fileEndings
import scala.tools.util.PathResolver
import io.Path
import settings.FscSettings

/** The client part of the fsc offline compiler.  Instead of compiling
 *  things itself, it send requests to a CompileServer.
 */
class StandardCompileClient extends HasCompileSocket {
  lazy val compileSocket: CompileSocket = CompileSocket

  val versionMsg  = "Fast " + Properties.versionMsg
  var verbose = false

  def main0(argsIn: Array[String]): Int = {
    // TODO: put -J -and -D options back.  Right now they are lost
    // because bash parses them out and they don't arrive.
    val (vmArgs, fscArgs) = (Nil, argsIn.toList)
    val settings = new FscSettings
    val command  = new CompilerCommand(fscArgs, settings)
    verbose = settings.verbose.value
    val shutdown = settings.shutdown.value

    if (settings.version.value) {
      Console println versionMsg
      return 0
    }
    if (verbose) {
      Console println fscArgs.mkString("[Given arguments: ", " ", "]")
      Console println vmArgs.mkString("[VM arguments: ", " ", "]")
    }
    val socket =
      if (settings.server.value == "") compileSocket.getOrCreateSocket(vmArgs mkString " ", !shutdown)
      else Some(compileSocket.getSocket(settings.server.value))

    val success = socket match {
      case Some(sock) => compileOnServer(sock, fscArgs)
      case _          =>
        Console.println(
          if (shutdown) "[No compilation server running.]"
          else "Compilation failed."
        )
        shutdown
    }
    if (success) 1 else 0
  }
}

object CompileClient extends StandardCompileClient {
  def main(args: Array[String]): Unit = sys exit {
    try main0(args)
    catch { case _: Exception => 1 }
  }
}

