class LiteralNode(val value: Any)

object LiteralNode {
  // irrefutable
  def unapply(n: LiteralNode) = Some(n.value)
}

object Test extends App {
  ((new LiteralNode(false)): Any) match {
    case LiteralNode(true)  => println("uh-oh")
    case LiteralNode(false) => println("ok")
  }
}