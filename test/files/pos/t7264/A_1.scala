object Foo {
  object Values {
    implicit def fromInt(x: Int): Values = ???
  }
  trait Values
}
final class Foo(name: String) {
  def bar(values: Foo.Values): Bar = ???
}

trait Bar
