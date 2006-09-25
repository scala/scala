
package nactors.distributed

[serializable] abstract class Node;

[serializable] case class TcpNode(address: String, port: Int) extends Node;
[serializable] case class JxtaNode(group: String) extends Node;
