trait Foo[a];

module Bug {
  def foo[a](): Foo[a] = foo[a]();
  foo()[Int];
}

