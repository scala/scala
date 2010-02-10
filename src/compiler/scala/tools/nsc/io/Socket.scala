/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package io

import java.io.{ IOException, InputStreamReader, BufferedReader, PrintWriter }
import java.net.{ URL, MalformedURLException }
import java.net.{ InetAddress, Socket => JSocket }
import scala.util.control.Exception._

/** A skeletal only-as-much-as-I-need Socket wrapper.
 */
object Socket
{
  private val socketExceptions = List(classOf[IOException], classOf[SecurityException])

  class SocketBox(f: () => Socket) {
    def either: Either[Throwable, Socket] = catching(socketExceptions: _*) either f()
    def opt: Option[Socket] = catching(socketExceptions: _*) opt f()
  }

  def apply(host: InetAddress, port: Int) = new SocketBox(() => new Socket(new JSocket(host, port)))
  def apply(host: String, port: Int) = new SocketBox(() => new Socket(new JSocket(host, port)))
}

class Socket(jsocket: JSocket) {
  def getOutputStream() = jsocket.getOutputStream()
  def getInputStream() = jsocket.getInputStream()
  def getPort() = jsocket.getPort()
  def close() = jsocket.close()

  /** Creates an InputStream and applies the closure, automatically closing it on completion.
   */
  def applyReaderAndWriter[T](f: (BufferedReader, PrintWriter) => T): T = {
    val out = new PrintWriter(getOutputStream(), true)
    val in = new BufferedReader(new InputStreamReader(getInputStream()))
    try f(in, out)
    finally {
      in.close()
      out.close()
    }
  }
}