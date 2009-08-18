/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.util

import java.lang.System
import java.io.PrintWriter
import java.io.BufferedOutputStream
import java.io.{BufferedReader, InputStreamReader}
import java.io.IOException
import java.net.{ServerSocket, SocketException, SocketTimeoutException}

/** The abstract class <code>SocketServer</code> implements the server
 *  communication for the fast Scala compiler.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class SocketServer {

  def shutDown: Boolean
  def session()

  var out: PrintWriter = _
  var in: BufferedReader = _

  def fatal(msg: String): Nothing = {
    System.err.println(msg)
    exit(1)
  }

  private def warn(msg: String) {
    System.err.println(msg)
  }

  // called after a timeout is detected,
  // for SocketServer subclasses to perform
  // some cleanup, if any
  def timeout() {}

  val serverSocket =
    try new ServerSocket(0)
    catch { case e: IOException => fatal("Could not listen on any port; exiting.") }

  val port: Int = serverSocket.getLocalPort()

  def run() {
    try {
      // After 30 idle minutes, politely exit.
      // Should the port file disappear, and the clients
      // therefore unable to contact this server instance,
      // the process will just eventually terminate by itself.
      serverSocket.setSoTimeout(1800000)
    } catch {
      case e: SocketException =>
        fatal("Could not set timeout on port: " + port + "; exiting.")
    }
    try {
      while (!shutDown) {
        val clientSocket = try {
          serverSocket.accept()
        } catch {
          case e: IOException =>
            fatal("Accept on port " + port + " failed; exiting.")
        }

        out = new PrintWriter(clientSocket.getOutputStream(), true)
        in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()))
        val bufout = new BufferedOutputStream(clientSocket.getOutputStream, 10240)

        scala.Console.withOut(bufout) {
          session()
        }
        bufout.close()
        out.close()
        in.close()
        clientSocket.close()
      }
    } catch {
      case e: SocketTimeoutException =>
        warn("Timeout elapsed with no requests from clients on port " + port + "; exiting")
        timeout()
    }
    serverSocket.close()
  }
}
