abstract class Foo {
  type Inner;
  type Alias = Inner;
}

class Bar extends Foo {
  override type Inner = Alias;
}
