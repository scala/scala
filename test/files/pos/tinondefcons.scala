// Tests instantiating a type parameter when a non-default
// constructor is used.


class Atom[T](b: Boolean) {
  var elem: T = _;
  def this(s: T) = this(true)
}


object AtomTest {
  new Atom("hello").elem = "abc"
}
