class Base { trait T; object T extends T }
class Derived extends Base {
  class Inner {
    assert(T.isInstanceOf[Derived.super.T])
  }
}

object Test extends App {
  val d = new Derived
  new d.Inner
}
