abstract class Foo {
  type T;
}

class Bar(foo: Foo)  {
  def a: foo.T = a;
}
