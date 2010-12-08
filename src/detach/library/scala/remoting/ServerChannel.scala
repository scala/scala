/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ServerChannel.scala 18365 2009-07-21 11:00:42Z michelou $

package scala.remoting

import java.net.{ServerSocket, Socket}

/** <p>
 *    Creates a server channel and binds its associated socket to the
 *    specified port number.<br/>
 *    Example:
 *  </p><pre>
 *  <b>class</b> ComputeChannel(s: Socket) <b>extends</b> Channel(s) {
 *    <b>def</b> receiveFunc = receive[Int => Int]
 *  }
 *  <b>class</b> ComputeServer(p: Int)
 *  <b>extends</b> AbstractServerChannel[ComputeChannel](p) {
 *     <b>def</b> newChannel(s: Socket) = <b>new</b> ComputeChannel(s)
 *  }</pre>
 *
 *  @author Stephane Micheloud
 *  @version 1.0
 */
class ServerChannel(p: Int) extends AbstractServerChannel[Channel](p) {
  def newChannel(s: Socket) = new Channel(s)
}

abstract class AbstractServerChannel[T <: Channel](_port: Int) {

  /** Creates an input channel and binds its associated socket to any
   *  free port.
   */
  def this() = this(0)

  // The maximum queue length for incoming requests to connect is set to 50.
  private val serverSocket = new ServerSocket(_port)

  /** Returns the local address of this channel. */
  val host = serverSocket.getInetAddress.getHostAddress

  /** Returns the port on which this channel is listening. */
  val port = serverSocket.getLocalPort
  info("Listening on port "+port)

  protected def newChannel(socket: Socket): T

  def accept: T = {
    System.gc() // required!
    newChannel(serverSocket.accept)
  }

  def close() {
    try { serverSocket.close() }
    catch { case e: java.io.IOException => }
    info("Server socket "+host+":"+port+" closed")
  }

  protected def info(msg: String) {
    runtime.remoting.Debug.info("[ServerChannel] "+msg)
  }
}
