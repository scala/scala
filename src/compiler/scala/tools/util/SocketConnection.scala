package scala.tools.util

import java.io._
import java.net._

class SocketConnection(hostname: String, port: int) {

  def this(port: int) = this(InetAddress.getLocalHost().getHostName(), port)

  private var socket: Socket = _
  var out: PrintWriter = _
  var in: BufferedReader = _
  var errorMessage: String = _

  def open(): boolean = {
    try {
      socket = new Socket(hostname, port)
      out = new PrintWriter(socket.getOutputStream(), true)
      in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
      true
    } catch {
      case e: UnknownHostException =>
        errorMessage = "Don't know about host: "+hostname+"."
        false
      case e: IOException =>
        errorMessage = "Couldn't get I/O for the connection to: "+hostname+"."
        false
    }
  }

  def close() = {
    in.close()
    out.close()
    socket.close()
  }
}

