/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package io

import java.io.{ IOException, InputStreamReader, BufferedReader, PrintWriter, Closeable }
import java.io.{ BufferedOutputStream, BufferedReader }
import java.net.{ InetAddress, Socket => JSocket }
import scala.io.Codec

/** A skeletal only-as-much-as-I-need Socket wrapper.
 */
object Socket {
  class Box[+T](f: () => T) {
    private def handlerFn[U](f: Throwable => U): PartialFunction[Throwable, U] = {
      case x @ (_: IOException | _: SecurityException)  => f(x)
    }
    private val optHandler = handlerFn[Option[T]](_ => None)
    private val eitherHandler = handlerFn[Either[Throwable, T]](x => Left(x))

    def either: Either[Throwable, T]    = try Right(f()) catch eitherHandler
    def opt: Option[T]                  = try Some(f()) catch optHandler
  }

  def localhost(port: Int)                = apply(InetAddress.getLocalHost(), port)
  def apply(host: InetAddress, port: Int) = new Box(() => new Socket(new JSocket(host, port)))
  def apply(host: String, port: Int)      = new Box(() => new Socket(new JSocket(host, port)))
}

class Socket(jsocket: JSocket) extends Streamable.Bytes with Closeable {
  def inputStream()  = jsocket.getInputStream()
  def outputStream() = jsocket.getOutputStream()
  def getPort()      = jsocket.getPort()
  def close()        = jsocket.close()

  def printWriter()                         = new PrintWriter(outputStream(), true)
  def bufferedReader(implicit codec: Codec) = new BufferedReader(new InputStreamReader(inputStream()))
  def bufferedOutput(size: Int)             = new BufferedOutputStream(outputStream(), size)

  /** Creates an InputStream and applies the closure, automatically closing it on completion.
   */
  def applyReaderAndWriter[T](f: (BufferedReader, PrintWriter) => T): T = {
    val out = printWriter()
    val in  = bufferedReader

    try f(in, out)
    finally {
      in.close()
      out.close()
    }
  }
}
