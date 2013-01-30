// SIP-15 was changed to allow nested classes. See run/t5882.scala

class NodeOps(val n: Any) extends AnyVal {
  case class Scope()
  object Bar
}
