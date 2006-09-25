/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import java.io.IOException
import java.net.{InetAddress,ServerSocket,Socket,UnknownHostException}

/**
 * @author Philipp Haller
 */
object TcpService {
  val random = new java.util.Random(System.currentTimeMillis())

  def generatePort: int = {
    var portnum = 0
    try {
      portnum = 8000 + random.nextInt(500)
      val socket = new ServerSocket(portnum)
      socket.close()
    }
    catch {
      case ioe: IOException =>
        // this happens when trying to open a socket twice at the same port
        // try again
        generatePort
      case se: SecurityException =>
        // do nothing
    }
    portnum
  }
}

object TestPorts {
  def main(args: Array[String]): Unit = {
    val random = new java.util.Random(System.currentTimeMillis())
    val socket = new ServerSocket(8000 + random.nextInt(500))
    Console.println(TcpService.generatePort)
  }
}

/**
 * @author Philipp Haller
 */
class TcpService(port: Int) extends Thread with Service {
  val serializer: JavaSerializer = new JavaSerializer(this)

  private val internalNode = new TcpNode(InetAddress.getLocalHost().getHostAddress(), port);
  def node: TcpNode = internalNode

  def createPid(actor: RemoteActor): RemotePid =
    new TcpPid(internalNode, makeUid, kernel, actor)

  def send(node: Node, data: String): unit = synchronized {
    // retrieve worker thread (if any) that already has connection
    node match {
      case tnode: TcpNode =>
        getConnection(tnode) match {
          case None =>
            // we are not connected, yet
            Console.println("We are not connected, yet.");
            val newWorker = connect(tnode); //bad in a sync BLOCK!!!
            newWorker transmit data
          case Some(worker) => worker transmit data
        }
      case any => error("no TcpNode!");
    }
  }

  def send(node: Node, data: Array[byte]): unit = synchronized {
    // retrieve worker thread (if any) that already has connection
    node match {
      case tnode: TcpNode =>
        getConnection(tnode) match {
          case None =>
            // we are not connected, yet
            Console.println("We are not connected, yet.");
            val newWorker = connect(tnode); //bad in a sync BLOCK!!!
            newWorker transmit data
          case Some(worker) => worker transmit data
        }
      case any => error("no TcpNode!");
    }
  }

  override def run(): Unit =
    try {
      val socket = new ServerSocket(port);
      Console.println("Tcp Service started: " + node);

      while (true) {
        val nextClient = socket.accept();
        Console.println("Received request from " + nextClient.getInetAddress() + ":" + nextClient.getPort());

        // this is bad because client will have other port than actual node
        // solution: worker should read node from stream
        // and call main thread to update connection table

        // spawn new worker thread
        val worker = new TcpServiceWorker(this, nextClient);
        worker.readNode;
        // start worker thread
        worker.start()
      }
    }
    catch {
      case ioe: IOException =>
        // do nothing
      case sec: SecurityException =>
        // do nothing
    }

  // connection management

  private val connections =
    new scala.collection.mutable.HashMap[TcpNode,TcpServiceWorker]

  def nodes: List[Node] =
    throw new Exception ("nodes need to be implemented in TcpService!")

  def addConnection(n: TcpNode, w: TcpServiceWorker) = synchronized {
    connections += n -> w
  }

  def getConnection(n: TcpNode) = synchronized {
    connections.get(n)
  }

  def isConnected(n: Node): Boolean = synchronized {
    n match {
      case tnode: TcpNode =>
        ! connections.get(tnode).isEmpty
      case _ =>
        false
    }
  }

  def connect(n: Node): Unit = synchronized {
    n match {
      case tnode: TcpNode =>
      	connect(tnode)
    }
  }

  def connect(n: TcpNode): TcpServiceWorker = synchronized {
    Console.println("" + node + ": Connecting to node " + n + " ...")
    val sock = new Socket(n.address, n.port)
    Console.println("Connected.")
    // spawn new worker thread
    val worker = new TcpServiceWorker(this, sock)
    worker.sendNode;
    // start worker thread
    worker.start()
    // register locally (we want to reuse connections which correspond to connected sockets)
    // update connection table
    addConnection(n, worker)
    worker
  }

  def disconnectNode(n: Node) = synchronized {
    n match {
      case node: TcpNode =>
        Console.println("Disconnecting from " + node + " ...")
        connections.get(node) match {
          case None => Console.println("Cannot disconnect from " + node + ". Not connected.")
          case Some(worker) =>
            //TODO: sending disconnect message
            //worker.sendDisconnect;
            // update table
            connections -= node
            Console.println("Halting worker...")
            worker.halt
        }
      case any => error("No TcpNode!!");
    }
  }

  def isReachable(node: Node): boolean =
    if (isConnected(node)) true
    else try {
      connect(node)
      return true
    }
    catch {
      case uhe: UnknownHostException =>
        false
      case ioe: IOException =>
        false
      case se: SecurityException =>
        false
    }

  def getRoundTripTimeMillis(node: Node): Long = 0

  def nodeDown(mnode: TcpNode): Unit = synchronized {
    kernel nodeDown mnode
    connections -= mnode
  }

  /*def closeConnection(worker: TcpServiceWorker): unit = synchronized {
    connections.get(worker) match {
      case None =>
        System.err.println("Worker " + worker + " not registered.");
      case Some(socket) => {
        try {
          socket.close();
          connections -= worker
        }
        catch {
          case ioe:IOException =>
            System.err.println("Couldn't close connection.");
            connections -= worker
        }
      }
    };
    System.out.println("OK. Connection closed.")
  }*/

}
