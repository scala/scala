trait Foo[a];

object Bug {
  def foo[a](): Foo[a] = foo[a]();
  foo()[Int];
}

