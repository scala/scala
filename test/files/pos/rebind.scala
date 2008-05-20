abstract class Foo {
  class Inner {
    def inner: Int = 1
  }
  def foo: Inner
}
trait Bar {
  type Inner
  def foo: Inner = foo
}
class Test extends Foo with Bar {
  println(foo.inner)
}
