package scala.actors.distributed;

import java.io.StringWriter;

trait Service {
  val serializer: Serializer;
  def node: Node;
  def createPid(actor: RemoteActor): RemotePid;
  def send(node: Node, data: Array[byte]): unit;
  def connect(node: Node): unit; // non blocking.
  def disconnectNode(node: Node): unit;
  def isConnected(node: Node): boolean;

  //blocking. timeout depends on Implementation.
  def isReachable(node: Node): boolean;

  def getRoundTripTimeMillis(node:Node): long; //blocking

  def nodes:List[Node]

// implemented parts:

  private val kern = new NetKernel(this);
  def kernel = kern;

  def spawn(name: String): RemotePid =
    kern spawn name;

  def spawn(name: String, arg: RemotePid): RemotePid =
    kern.spawn(name, arg);


  //suggested addition by seb
  def spawn(fun: RemoteActor => unit): RemotePid =
    kernel.spawn(fun);
  def spawn(a:RemoteActor):RemotePid = {
    //Console.println("Service:spawn(RemoteActor)")
    val pid = kernel.register(a)
    //Console.println("RemoteActor("+a+") registered in kernel")
    a.start
    //Console.println("RemoteActor("+a+") started")
    pid
  }

  def send(pid: RemotePid, msg: AnyRef): unit = synchronized {
    if (pid.node == this.node)
      kernel.localSend(pid, msg)
    else
      kernel.remoteSend(pid, msg)
  }

  def remoteSend(pid: RemotePid, msg: AnyRef): unit = synchronized {
    //Console.println("Service: Sending " + msg + " to " + pid)
    // lets try to serialize the message
    //val sw = new StringWriter
    //serializer.serialize(msg, sw)
    val bytes = serializer.serialize(msg)
    //val sendMsg = Send(pid, sw.toString())
    val sendMsg = Send(pid, bytes)
    //val sw2 = new StringWriter
    //serializer.serialize(sendMsg, sw2)
    //send(pid.node, sw2.toString())
    val bytes2 = serializer.serialize(sendMsg)
    send(pid.node, bytes2)
  }

  private var idCnt = 0;
  def makeUid = { idCnt = idCnt + 1; idCnt }



}
