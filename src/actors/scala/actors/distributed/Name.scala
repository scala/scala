package scala.actors.distributed;

case class Name(node: Node, sym: Symbol, kernel: NetKernel) {
  def !(msg: AnyRef): unit = {
    kernel.namedSend(this, msg)
  }
}
