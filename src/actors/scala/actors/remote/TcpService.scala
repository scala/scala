/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors.remote


import java.lang.{Thread, SecurityException}
import java.io.{DataInputStream, DataOutputStream, IOException}
import java.net.{InetAddress, ServerSocket, Socket, UnknownHostException}

import compat.Platform

import scala.collection.mutable.HashMap

/* Object TcpService.
 *
 * @version 0.9.8
 * @author Philipp Haller
 */
object TcpService {
  val random = new java.util.Random(Platform.currentTime)

  def generatePort: int = {
    var portnum = 0
    try {
      portnum = 8000 + random.nextInt(500)
      val socket = new ServerSocket(portnum)
      socket.close()
    }
    catch {
      case ioe: IOException =>
        // this happens when trying to open a socket twice
        // at the same port
        // try again
        generatePort
      case se: SecurityException =>
        // do nothing
    }
    portnum
  }
}

/* Class TcpService.
 *
 * @version 0.9.8
 * @author Philipp Haller
 */
class TcpService(port: Int) extends Thread with Service {
  val serializer: JavaSerializer = new JavaSerializer(this)

  private val internalNode = new Node(InetAddress.getLocalHost().getHostAddress(), port)
  def node: Node = internalNode

  private val pendingSends = new HashMap[Node, List[Array[byte]]]

  def send(node: Node, data: Array[byte]): unit = synchronized {

    def bufferMsg(t: Throwable) = {
      // buffer message, so that it can be re-sent
      // when remote net kernel comes up
      pendingSends.get(node) match {
        case None =>
          pendingSends += node -> (data :: Nil)
        case Some(msgs) =>
          pendingSends += node -> (data :: msgs)
      }
    }

    // retrieve worker thread (if any) that already has connection
    getConnection(node) match {
      case None =>
        // we are not connected, yet
        try {
          val newWorker = connect(node)
          newWorker transmit data

          // any pending sends?
          pendingSends.get(node) match {
            case None =>
              // do nothing
            case Some(msgs) =>
              msgs foreach {newWorker transmit _}
              pendingSends -= node
          }
        } catch {
          case uhe: UnknownHostException =>
            bufferMsg(uhe)
          case ioe: IOException =>
            bufferMsg(ioe)
          case se: SecurityException =>
            // do nothing
        }
      case Some(worker) =>
        worker transmit data
    }
  }

  override def run() {
    try {
      val socket = new ServerSocket(port)
      while (true) {
        Debug.info(this+": waiting for new connection...")
        val nextClient = socket.accept()
        val worker = new TcpServiceWorker(this, nextClient)
        Debug.info("Started new "+worker)
        worker.readNode
        worker.start()
      }
    } catch {
      case ioe: IOException =>
        Debug.info(this+": caught "+ioe)
      case sec: SecurityException =>
        Debug.info(this+": caught "+sec)
      case e: Exception =>
        Debug.info(this+": caught "+e)
    } finally {
      Debug.info(this+": shutting down...")
    }
  }

  // connection management

  private val connections =
    new scala.collection.mutable.HashMap[Node, TcpServiceWorker]

  private[actors] def addConnection(node: Node, worker: TcpServiceWorker) = synchronized {
    connections += node -> worker
  }

  def getConnection(n: Node) = synchronized {
    connections.get(n)
  }

  def isConnected(n: Node): Boolean = synchronized {
    !connections.get(n).isEmpty
  }

  def connect(n: Node): TcpServiceWorker = synchronized {
    val sock = new Socket(n.address, n.port)
    val worker = new TcpServiceWorker(this, sock)
    worker.sendNode(n)
    worker.start()
    addConnection(n, worker)
    worker
  }

  def disconnectNode(n: Node) = synchronized {
    connections.get(n) match {
      case None => {
        // do nothing
      }
      case Some(worker) => {
        connections -= n
        worker.halt
      }
    }
  }

  def isReachable(node: Node): boolean =
    if (isConnected(node)) true
    else try {
      connect(node)
      return true
    } catch {
      case uhe: UnknownHostException => false
      case ioe: IOException => false
      case se: SecurityException => false
    }

  def nodeDown(mnode: Node): Unit = synchronized {
    connections -= mnode
  }
}


class TcpServiceWorker(parent: TcpService, so: Socket) extends Thread {
  val in = so.getInputStream()
  val out = so.getOutputStream()

  val datain = new DataInputStream(in)
  val dataout = new DataOutputStream(out)

  var connectedNode: Node = _

  def sendNode(n: Node) = {
    connectedNode = n
    parent.serializer.writeObject(dataout, parent.node)
  }

  def readNode = {
    val node = parent.serializer.readObject(datain)
    node match {
      case n: Node => {
        connectedNode = n
        parent.addConnection(n, this)
      }
    }
  }

  def transmit(data: Array[byte]): Unit = synchronized {
    Debug.info(this+": transmitting data...")
    dataout.writeInt(data.length)
    dataout.write(data)
    dataout.flush()
  }

  var running = true

  def halt = synchronized {
    so.close()
    running = false
  }

  override def run() {
    try {
      while (running) {
        val msg = parent.serializer.readObject(datain);
        parent.kernel.processMsg(connectedNode, msg)
      }
    }
    catch {
      case ioe: IOException =>
        Debug.info(this+": caught "+ioe)
        parent nodeDown connectedNode
      case e: Exception =>
        Debug.info(this+": caught "+e)
        parent nodeDown connectedNode
    }
    Debug.info(this+": shutting down...")
  }
}
