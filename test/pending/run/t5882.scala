// SIP-15 was revised to allow nested classes in value classes.
// This test checks that their basic functionality.

class NodeOps(val n: Any) extends AnyVal { self =>
  class Foo() { def show = self.show(n) }
  def show(x: Any) = x.toString
}


object Test extends App {

  val n = new NodeOps("abc")
  assert(new n.Foo().show == "abc")
}
