case class Event[T](sender: T)

class Foo(bar: String) {
  self =>

  def this() {
    this("baz")

    new { Event(self) } // crashes compiler
  }
}

class Bar(x: Int) {
  def this() = {
    this(1)
    new { x }
  }
}
