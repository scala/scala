package scala.actors.distributed;

abstract class JXTAServiceBase(nodename: String) extends Thread with Service {
  val serializer = new JavaSerializer(this);
  private val internalNode = new JXTANode(nodename);
  def node: Node = internalNode;
  def createPid(actor: RemoteActor): RemotePid =
    new JXTAPid(internalNode, makeUid, kernel, actor)
}
