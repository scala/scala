package scala.actors.remote

import java.io.{DataInputStream,DataOutputStream,BufferedReader,PrintWriter,
                IOException,InputStreamReader,OutputStreamWriter}
import java.net.{InetAddress,ServerSocket,Socket,UnknownHostException}

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

class TcpService(port: Int) extends Thread with Service {
  val serializer: JavaSerializer = new JavaSerializer(this)

  private val internalNode = new TcpNode(InetAddress.getLocalHost().getHostAddress(), port)
  def node: TcpNode = internalNode

  def send(node: Node, data: Array[byte]): unit = synchronized {
    // retrieve worker thread (if any) that already has connection
    node match {
      case tnode: TcpNode =>
        getConnection(tnode) match {
          case None =>
            // we are not connected, yet
            val newWorker = connect(tnode)
            newWorker transmit data
          case Some(worker) => worker transmit data
        }
      case any => error("no TcpNode!")
    }
  }

  override def run(): Unit =
    try {
      val socket = new ServerSocket(port)

      while (true) {
        val nextClient = socket.accept()
        val worker = new TcpServiceWorker(this, nextClient)
        worker.readNode
        worker.start()
      }
    } catch {
      case ioe: IOException => // do nothing
      case sec: SecurityException => // do nothing
    }

  // connection management

  private val connections =
    new scala.collection.mutable.HashMap[TcpNode, TcpServiceWorker]

  private[actors] def addConnection(node: TcpNode, worker: TcpServiceWorker) = synchronized {
    connections += node -> worker
  }

  def getConnection(n: TcpNode) = synchronized {
    connections.get(n)
  }

  def isConnected(n: Node): Boolean = synchronized {
    n match {
      case tnode: TcpNode => !connections.get(tnode).isEmpty
      case _ => false
    }
  }

  def connect(n: Node): Unit = synchronized {
    n match {
      case tnode: TcpNode =>
      	connect(tnode)
    }
  }

  def connect(n: TcpNode): TcpServiceWorker = synchronized {
    val sock = new Socket(n.address, n.port)
    val worker = new TcpServiceWorker(this, sock)
    worker.sendNode(n)
    worker.start()
    addConnection(n, worker)
    worker
  }

  def disconnectNode(n: Node) = synchronized {
    n match {
      case node: TcpNode =>
        connections.get(node) match {
          case None => // do nothing
          case Some(worker) => {
            connections -= node
            worker.halt
          }
        }
      case any => error("no TcpNode.")
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

  def nodeDown(mnode: TcpNode): Unit = synchronized {
    connections -= mnode
  }
}


class TcpServiceWorker(parent: TcpService, so: Socket) extends Thread {
  val in = so.getInputStream()
  val out = so.getOutputStream()

  val datain = new DataInputStream(in)
  val dataout = new DataOutputStream(out)

  val reader = new BufferedReader(new InputStreamReader(in))
  val writer = new PrintWriter(new OutputStreamWriter(out))

  var connectedNode: TcpNode = _

  def sendNode(n: TcpNode) = {
    connectedNode = n
    parent.serializer.writeObject(dataout, parent.node)
  }

  def readNode = {
    //val node = parent.serializer.deserialize(reader)
    val node = parent.serializer.readObject(datain)
    node match {
      case n: TcpNode => {
        connectedNode = n
        parent.addConnection(n, this)
      }
    }
  }

  def transmit(data: Array[byte]): Unit = synchronized {
    dataout.writeInt(data.length)
    dataout.write(data)
    dataout.flush()
  }

  var running = true

  def halt = synchronized {
    so.close()
    running = false
  }

  override def run(): Unit = {
    try {
      while (running) {
        if (in.available() > 0) {
          //val msg = parent.serializer.deserialize(reader);
          val msg = parent.serializer.readObject(datain);
          parent.kernel.processMsg(connectedNode, msg)
        }
      }
    }
    catch {
      case ioe: IOException =>
        parent nodeDown connectedNode
      case e: Exception =>
        // catch-all
        parent nodeDown connectedNode
    }
  }
}
