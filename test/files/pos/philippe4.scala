trait Foo[t <: Foo[t]] { self: t =>
  def foo(that: t): Boolean;
}
