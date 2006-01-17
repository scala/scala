trait Foo[t <: Foo[t]] requires t {
  def foo(that: t): Boolean;
}
