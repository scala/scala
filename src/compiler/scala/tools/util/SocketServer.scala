/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package tools.util

import java.net.{ ServerSocket, SocketException, SocketTimeoutException }
import java.io.{ PrintWriter, BufferedReader }
import scala.tools.nsc.io.Socket

trait CompileOutputCommon {
  def verbose: Boolean

  def info(msg: String)  = if (verbose) echo(msg)
  def echo(msg: String)  = {Console println msg; Console.flush()}
  def warn(msg: String)  = {Console.err println msg; Console.flush()}
  def fatal(msg: String) = { warn(msg) ; sys.exit(1) }
}

/** The abstract class SocketServer implements the server
 *  communication for the fast Scala compiler.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class SocketServer(fixPort: Int = 0) extends CompileOutputCommon {
  def shutdown: Boolean
  def session(): Unit
  def timeout(): Unit = ()  // called after a timeout is detected for subclasses to cleanup
  // a hook for subclasses
  protected def createServerSocket(): ServerSocket = new ServerSocket(fixPort)

  var in: BufferedReader = _
  var out: PrintWriter   = _
  val BufferSize         = 10240
  lazy val serverSocket  = createServerSocket()
  lazy val port          = serverSocket.getLocalPort()

  // Default to 30 minute idle timeout, settable with -max-idle
  protected var idleMinutes = 30
  private var savedTimeout = 0
  private val acceptBox = new Socket.Box(() => {
    // update the timeout if it has changed
    if (savedTimeout != idleMinutes) {
      savedTimeout = idleMinutes
      setTimeoutOnSocket(savedTimeout)
    }
    new Socket(serverSocket.accept())
  })
  private def setTimeoutOnSocket(mins: Int) = {
    try {
      serverSocket setSoTimeout (mins * 60 * 1000)
      info("Set socket timeout to " + mins + " minutes.")
      true
    }
    catch {
      case ex: SocketException =>
        warn("Failed to set socket timeout: " + ex)
        false
    }
  }

  def doSession(clientSocket: Socket) = {
    clientSocket.applyReaderAndWriter { (in, out) =>
      this.in    = in
      this.out   = out
      val bufout = clientSocket.bufferedOutput(BufferSize)

      try scala.Console.withOut(bufout)(session())
      finally bufout.close()
    }
  }

  def run() {
    info("Starting SocketServer run() loop.")

    def loop() {
      acceptBox.either match {
        case Right(clientSocket) =>
          try doSession(clientSocket)
          finally clientSocket.close()
        case Left(_: SocketTimeoutException) =>
          warn("Idle timeout exceeded on port %d; exiting" format port)
          timeout()
          return
        case _ =>
          warn("Accept on port %d failed")
      }
      if (!shutdown)
        loop()
    }
    try loop()
    catch { case ex: SocketException => fatal("Compile server caught fatal exception: " + ex) }
    finally serverSocket.close()
  }
}
