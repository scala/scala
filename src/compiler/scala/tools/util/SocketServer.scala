package scala.tools.util

import java.net._
import java.io._

abstract class SocketServer {

  def shutDown: boolean
  def session(): unit

  var out: PrintWriter = _
  var in: BufferedReader = _

  val port: int = try {
    val s = new ServerSocket(0)
    val p = s.getLocalPort()
    s.close()
    p
  } catch {
    case e: IOException =>
      System.err.println("Could not listen on any port; exiting.")
      exit(1)
  }

  def run(): unit = {
    while (!shutDown) {
      val serverSocket = try {
        new ServerSocket(port)
      } catch {
        case e: IOException =>
          System.err.println("Could not listen on port: "+port+"; exiting.")
          exit(1)
      }
      val clientSocket = try {
        serverSocket.accept()
      } catch {
        case e: IOException =>
          System.err.println("Accept on port "+port+" failed; exiting.")
          exit(1)
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

