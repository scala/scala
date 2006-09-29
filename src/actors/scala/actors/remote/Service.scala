package scala.actors.remote

trait Service {
  val kernel = new NetKernel(this)
  val serializer: Serializer
  def node: Node
  def send(node: Node, data: Array[Byte]): Unit
}
