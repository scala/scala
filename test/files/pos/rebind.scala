abstract class Foo {
  class Inner {
    def inner: int = 1;
  }
  def foo: Inner;
}
abstract class Bar {
  type Inner;
  def foo: Inner = foo;
}
class Test extends Foo with Bar {
  System.out.println(foo.inner);
}
