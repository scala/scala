/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.{ BufferedReader, File, InputStreamReader, PrintWriter }
import settings.FscSettings
import scala.tools.util.CompileOutputCommon
import sys.SystemProperties.preferIPv4Stack

/** The client part of the fsc offline compiler.  Instead of compiling
 *  things itself, it send requests to a CompileServer.
 */
class StandardCompileClient extends HasCompileSocket with CompileOutputCommon {
  lazy val compileSocket: CompileSocket = CompileSocket

  val versionMsg  = "Fast " + Properties.versionMsg
  var verbose = false

  def process(args: Array[String]): Int = {
    val fscArgs = args.toList
    val settings = new FscSettings
    val command  = new CompilerCommand(fscArgs, settings)
    verbose = settings.verbose.value
    val shutdown = settings.shutdown.value
    val vmArgs = settings.jvmargs.unparse ++ settings.defines.unparse ++ (
      if (settings.preferIPv4.value) List("-D%s=true".format(preferIPv4Stack.key)) else Nil
    )

    if (settings.version.value) {
      Console println versionMsg
      return 0
    }

    info(versionMsg)
    info(fscArgs.mkString("[Given arguments: ", " ", "]"))
    info(vmArgs.mkString("[VM arguments: ", " ", "]"))

    val socket =
      if (settings.server.value == "") compileSocket.getOrCreateSocket(vmArgs mkString " ", !shutdown)
      else Some(compileSocket.getSocket(settings.server.value))

    val success = socket match {
      case Some(sock) => compileOnServer(sock, fscArgs)
      case _          =>
        echo(
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
    try process(args)
    catch { case _: Exception => 1 }
  }
}

