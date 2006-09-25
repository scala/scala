
package nactors.distributed

import java.io.StringWriter

trait Service {
  private val kern = new NetKernel(this)
  def kernel = kern

  val serializer: Serializer
  def node: Node

  def send(node: Node, data: Array[byte]): Unit

/*
  def connect(node: Node): Unit // non-blocking
  def disconnectNode(node: Node): Unit
  def isConnected(node: Node): Boolean
  def isReachable(node: Node): Boolean // blocking
  def getRoundTripTimeMillis(node: Node): Long // blocking
  def nodes: List[Node]
*/
}
