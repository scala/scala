/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Channel.scala 18365 2009-07-21 11:00:42Z michelou $

package scala.remoting

import java.io._
import java.net._
import java.rmi.server.RMIClassLoader

/** <p>
 *    The class <code>Channel</code> implements (basic) typed channels
 *    which use <a href="http://java.sun.com/docs/books/tutorial/networking/sockets/"
 *    target="_top"/>Java socket</a> communication and Scala type manifests to
 *    provide type-safe send/receive operations between a localhost and another
 *    remote machine by specifying some <code>host</code> and <code>port</code>.
 *  </p>
 *
 *  @author Stephane Micheloud
 *  @version 1.1
 */
class Channel protected (socket: Socket) {

  // Create a socket without a timeout
  def this(host: String, port: Int) = this(new Socket(host, port))

  // // Create a socket with a timeout
  // val sockaddr: SocketAddress = new InetSocketAddress(addr, port)
  // val socket = new Socket()
  // // If the timeout occurs, SocketTimeoutException is thrown.
  // socket.connect(sockaddr, 2000) // 2 seconds

  /** Returns the local address of this channel. */
  val host = socket.getInetAddress.getHostAddress

  /** Returns the port on which this channel is listening. */
  val port = socket.getLocalPort

  private var cl: ClassLoader =
    try {
      // requires permission in Java policy file
      val codebase = System.getProperty("java.rmi.server.codebase")
      if (codebase != null) info("codebase="+codebase)
      RMIClassLoader.getClassLoader(codebase)
    }
    catch {
      case e: Exception =>
        sys.error("Class loader undefined: " + e.getMessage)
        null
    }
  def classLoader: ClassLoader = cl
  def classLoader_=(x: ClassLoader) { cl = x }

  info(""+this)

  private class CustomObjectInputStream(in: InputStream)
  extends ObjectInputStream(in) {
    override def resolveClass(desc: ObjectStreamClass): Class[_] =
      if (cl eq null)
        super.resolveClass(desc)
      else
        try {
          info("resolve class "+desc.getName)
          cl loadClass desc.getName
        }
        catch {
          case e: ClassNotFoundException =>
            super.resolveClass(desc)
        }
  }

  // lazy modifier is required!
  private lazy val in =
    try {
      new CustomObjectInputStream(socket.getInputStream)
    }
    catch {
      case e: IOException =>
        sys.error("Input stream undefined: "+e.getMessage+" ("+this+")")
        null
    }
  private lazy val out =
    try {
      new ObjectOutputStream(socket.getOutputStream)
    }
    catch {
      case e: IOException =>
        sys.error("Output stream undefined: "+e.getMessage+" ("+this+")")
        null
    }

  /** <code>receive&lt;primtype&gt;</code> methods may throw an
   *  <code>IOException</code>.
   */
  def receiveUnit    = receive[Unit]
  def receiveBoolean = receive[Boolean]
  def receiveByte    = receive[Byte]
  def receiveChar    = receive[Char]
  def receiveShort   = receive[Short]
  def receiveInt     = receive[Int]
  def receiveLong    = receive[Long]
  def receiveFloat   = receive[Float]
  def receiveDouble  = receive[Double]
  def receiveString  = receive[String]

  /** <code>receive</code> method may throw either an
   *  <code>ClassNotFoundException</code> or an <code>IOException</code>.
   *
   *  @throw <code>ChannelException</code> if received value has not
   *         the expected type.
   */
  @throws(classOf[ChannelException])
  def receive[T](implicit expected: reflect.ClassTag[T]): T = {
    val found = in.readObject().asInstanceOf[reflect.ClassTag[_]]
    info("receive: found="+found+", expected="+expected)
    import scala.reflect.ClassTag
    val x = found match {
      case ClassTag.Unit    => ()
      case ClassTag.Boolean => in.readBoolean()
      case ClassTag.Byte    => in.readByte()
      case ClassTag.Char    => in.readChar()
      case ClassTag.Short   => in.readShort()
      case ClassTag.Int     => in.readInt()
      case ClassTag.Long    => in.readLong()
      case ClassTag.Float   => in.readFloat()
      case ClassTag.Double  => in.readDouble()
      case _                => in.readObject()
    }
    val res = if (found <:< expected)
      x.asInstanceOf[T]
    else
      throw new ChannelException(
        "\n\tfound \""+found+"\"\n\texpected \""+expected+"\"")
    info("received "+res+" (available="+in.available+")")
    res
  }

  /** <code>?</code> method may throw either an
   *  <code>ClassNotFoundException</code> or an <code>IOException</code>.
   */
  def ?[T](implicit t: reflect.ClassTag[T]): T = receive[T](t)

  /** <code>send</code> method may throw an <code>IOException</code>.
   */
  def send[T](x: T)(implicit t: reflect.ClassTag[T]) {
    out writeObject t
    x match {
      case x: Unit    => // nop
      case x: Boolean => out writeBoolean x
      case x: Byte    => out writeByte x
      case x: Char    => out writeChar x
      case x: Short   => out writeShort x
      case x: Int     => out writeInt x
      case x: Long    => out writeLong x
      case x: Float   => out writeFloat x
      case x: Double  => out writeDouble x
      case x          => out writeObject x
    }
    out.flush()
    info("sent "+x)
  }

  /** <code>!</code> method may throw an <code>IOException</code>.
   */
  def ![T](x: T)(implicit m: reflect.ClassTag[T]) { send(x)(m) }

  def close() {
    try { socket.close() }
    catch { case e: IOException => }
    info(this+" closed")
  }

  override def toString: String = socket.toString

  private def info(msg: String) {
    runtime.remoting.Debug.info("[Channel] "+msg)
  }
}

/** <code>ChannelException</code> may be thrown by the operation
 *  <code>receive</code> when the received data has not the expected type.
 */
case class ChannelException(msg: String) extends IOException(msg)

