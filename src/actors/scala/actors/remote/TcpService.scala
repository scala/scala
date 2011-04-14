/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.actors
package remote


import java.io.{DataInputStream, DataOutputStream, IOException}
import java.lang.{Thread, SecurityException}
import java.net.{InetAddress, ServerSocket, Socket, UnknownHostException}

import scala.collection.mutable.HashMap
import scala.util.Random

/* Object TcpService.
 *
 * @version 0.9.9
 * @author Philipp Haller
 */
object TcpService {
  private val random = new Random
  private val ports = new HashMap[Int, TcpService]

  def apply(port: Int, cl: ClassLoader): TcpService =
    ports.get(port) match {
      case Some(service) =>
        service
      case None =>
        val service = new TcpService(port, cl)
        ports += Pair(port, service)
        service.start()
        Debug.info("created service at "+service.node)
        service
    }

  def generatePort: Int = {
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

  var BufSize: Int = 65536
}

/* Class TcpService.
 *
 * @version 0.9.10
 * @author Philipp Haller
 */
class TcpService(port: Int, cl: ClassLoader) extends Thread with Service {
  val serializer: JavaSerializer = new JavaSerializer(this, cl)

  private val internalNode = new Node(InetAddress.getLocalHost().getHostAddress(), port)
  def node: Node = internalNode

  private val pendingSends = new HashMap[Node, List[Array[Byte]]]

  /**
   * Sends a byte array to another node on the network.
   * If the node is not yet up, up to <code>TcpService.BufSize</code>
   * messages are buffered.
   */
  def send(node: Node, data: Array[Byte]): Unit = synchronized {

    def bufferMsg(t: Throwable) {
      // buffer message, so that it can be re-sent
      // when remote net kernel comes up
      (pendingSends.get(node): @unchecked) match {
        case None =>
          pendingSends += Pair(node, List(data))
        case Some(msgs) if msgs.length < TcpService.BufSize =>
          pendingSends += Pair(node, data :: msgs)
      }
    }

    // retrieve worker thread (if any) that already has connection
    getConnection(node) match {
      case None =>
        // we are not connected, yet
        try {
          val newWorker = connect(node)

          // any pending sends?
          pendingSends.get(node) match {
            case None =>
              // do nothing
            case Some(msgs) =>
              msgs.reverse foreach {newWorker transmit _}
              pendingSends -= node
          }

          newWorker transmit data
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

  def terminate() {
    shouldTerminate = true
    try {
      new Socket(internalNode.address, internalNode.port)
    } catch {
      case ce: java.net.ConnectException =>
        Debug.info(this+": caught "+ce)
    }
  }

  private var shouldTerminate = false

  override def run() {
    try {
      val socket = new ServerSocket(port)
      while (!shouldTerminate) {
        Debug.info(this+": waiting for new connection on port "+port+"...")
        val nextClient = socket.accept()
        if (!shouldTerminate) {
          val worker = new TcpServiceWorker(this, nextClient)
          Debug.info("Started new "+worker)
          worker.readNode
          worker.start()
        } else
          nextClient.close()
      }
    } catch {
      case e: Exception =>
        Debug.info(this+": caught "+e)
    } finally {
      Debug.info(this+": shutting down...")
      connections foreach { case (_, worker) => worker.halt }
    }
  }

  // connection management

  private val connections =
    new scala.collection.mutable.HashMap[Node, TcpServiceWorker]

  private[actors] def addConnection(node: Node, worker: TcpServiceWorker) = synchronized {
    connections += Pair(node, worker)
  }

  def getConnection(n: Node) = synchronized {
    connections.get(n)
  }

  def isConnected(n: Node): Boolean = synchronized {
    !connections.get(n).isEmpty
  }

  def connect(n: Node): TcpServiceWorker = synchronized {
    val socket = new Socket(n.address, n.port)
    val worker = new TcpServiceWorker(this, socket)
    worker.sendNode(n)
    worker.start()
    addConnection(n, worker)
    worker
  }

  def disconnectNode(n: Node) = synchronized {
    connections.get(n) match {
      case None =>
        // do nothing
      case Some(worker) =>
        connections -= n
        worker.halt
    }
  }

  def isReachable(node: Node): Boolean =
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


private[actors] class TcpServiceWorker(parent: TcpService, so: Socket) extends Thread {
  val datain = new DataInputStream(so.getInputStream)
  val dataout = new DataOutputStream(so.getOutputStream)

  var connectedNode: Node = _

  def sendNode(n: Node) {
    connectedNode = n
    parent.serializer.writeObject(dataout, parent.node)
  }

  def readNode {
    val node = parent.serializer.readObject(datain)
    node match {
      case n: Node =>
        connectedNode = n
        parent.addConnection(n, this)
    }
  }

  def transmit(data: Array[Byte]): Unit = synchronized {
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
    Debug.info(this+": service terminated at "+parent.node)
  }
}
