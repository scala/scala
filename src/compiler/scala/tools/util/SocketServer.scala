/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.tools.util

import java.io.{ PrintWriter, BufferedOutputStream, BufferedReader, InputStreamReader, IOException }
import java.net.{ Socket, ServerSocket, SocketException, SocketTimeoutException }

object SocketServer {
  val BufferSize  = 10240

  def bufferedReader(s: Socket) = new BufferedReader(new InputStreamReader(s.getInputStream()))
  def bufferedOutput(s: Socket) = new BufferedOutputStream(s.getOutputStream, BufferSize)
}
import SocketServer._

/** The abstract class <code>SocketServer</code> implements the server
 *  communication for the fast Scala compiler.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class SocketServer {
  // After some number of idle minutes, politely exit.
  // Should the port file disappear, and the clients
  // therefore unable to contact this server instance,
  // the process will just eventually terminate by itself.
  def fscIdleMinutes = {
    sys.props("scala.config.fsc.idle-minutes") match {
      case null   => 30
      case str    => try str.toInt catch { case _: Exception => 30 }
    }
  }
  def fscIdleMillis = fscIdleMinutes * 60 * 1000

  def shutdown: Boolean
  def session(): Unit

  var out: PrintWriter = _
  var in: BufferedReader = _

  def fatal(msg: String): Nothing = {
    System.err.println(msg)
    sys.exit(1)
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

  // @todo: this is going to be a prime candidate for ARM
  def doSession(clientSocket: Socket) = {
    out = new PrintWriter(clientSocket.getOutputStream(), true)
    in  = bufferedReader(clientSocket)
    val bufout = bufferedOutput(clientSocket)

    try scala.Console.withOut(bufout)(session())
    finally {
      bufout.close()
      out.close()
      in.close()
    }
  }

  def run() {
    def fail(s: String) = fatal(s format port)
    Console.println("Setting timeout to " + fscIdleMillis)
    try serverSocket setSoTimeout fscIdleMillis catch {
      case e: SocketException => fatal("Could not set timeout on server socket; exiting.")
    }

    try {
      while (!shutdown) {
        val clientSocket = try serverSocket.accept() catch {
          case e: IOException => fail("Accept on port %d failed; exiting.")
        }
        try doSession(clientSocket)
        finally clientSocket.close()
      }
    }
    catch {
      case e: SocketTimeoutException =>
        warn("Timeout elapsed with no requests from clients on port %d; exiting" format port)
        timeout()
    }
    finally serverSocket.close()
  }
}
