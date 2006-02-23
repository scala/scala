// Tests instantiating a type parameter when a non-default
// constructor is used.


class Atom[T](b: Boolean) {
  def this(s: T) = this(true)
}


object AtomTest {
  val x = new Atom("hello")
}
