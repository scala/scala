class Exception1 extends RuntimeException
class Exception2 extends RuntimeException

class Foo_1 {
  def foo(baz: Baz) {
    try {
      baz.bar
    } catch {
      case _: Exception1 => println("exception 1")
      case _: Exception2 => println("exception 2")
    } finally {
      // this should be the only copy of the magic constant 3
      // making it easy to detect copies of this finally block
      println(s"finally ${3}")
    }
    println(s"normal flow")
  }
}

trait Baz {
  // does it throw? who knows? This way
  // I can ensure that no optimization that honors
  // separate compilation could ever
  // change the exception handling structure
  def bar: Unit
}
