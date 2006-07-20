/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.util

import java.net._
import java.io._

/** This abstract class implements the server communication for
 *  the fast Scala compiler.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class SocketServer {

  def shutDown: boolean
  def session(): unit

  var out: PrintWriter = _
  var in: BufferedReader = _

  def fatal(msg: String) = {
    System.err.println(msg)
    exit(1)
  }

  val port: int = try {
    val s = new ServerSocket(0) // a port of 0 creates a socket on any free port.
    val p = s.getLocalPort()
    s.close()
    p
  } catch {
    case e: IOException =>
      fatal("Could not listen on any port; exiting.")
  }

  def run(): unit = {
    while (!shutDown) {
      val serverSocket = try {
        new ServerSocket(port)
      } catch {
        case e: IOException =>
          fatal("Could not listen on port: " + port + "; exiting.")
      }
      val clientSocket = try {
        serverSocket.accept()
      } catch {
        case e: IOException =>
          fatal("Accept on port " + port + " failed; exiting.")
      }

      out = new PrintWriter(clientSocket.getOutputStream(), true)
      in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()))

      session()

      out.close()
      in.close()
      clientSocket.close()
      serverSocket.close()
    }
  }
}

