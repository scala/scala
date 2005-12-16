trait Foo[t <: Foo[t]]: t {
  def foo(that: t): Boolean;
}
