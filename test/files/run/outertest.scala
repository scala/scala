// A test for the case where the outer field of class B#J should be eliminated.
// You can verify this by running a javap on B.J
abstract class A {

  abstract class I {

  }

  val foo = "foo"

}

class B extends A {

  class J extends I {
    val bar = foo
  }

}

object  Test extends App {

  val b = new B
  assert((new b.J).bar == b.foo)

}
