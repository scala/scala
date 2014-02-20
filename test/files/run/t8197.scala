// NOTE: according to SI-4728, this shouldn't even compile...
class A
class B
// default arguments do not participate in overload resolution
class Foo(val x: A = null) {
  def this(bla: B*) {
    this(new A)
  }
}

object Test extends App {
  assert((new Foo).x != null)
}
