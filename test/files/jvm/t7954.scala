object Test extends App {
  new A { val foo = 1 }
  new B { val foo = 1 }
}

import annotation._

@strictfp trait A {
  def foo: Int
}

@strictfp abstract class B {
  def foo: Int
}
