package scala.actors.distributed;

[serializable] abstract class Node;

[serializable] case class TcpNode(address: String, port: int) extends Node;
[serializable] case class JXTANode(name: String) extends Node;
