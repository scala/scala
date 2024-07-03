object Node {
  trait Root { self: Node =>
    val root = this
  }
}
trait Node {
  def root: Node
}
final class RootNode extends Node with Node.Root
