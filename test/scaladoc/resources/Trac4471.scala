class A {
  @deprecated def foo = 123
  @deprecated def bar = 456
}

class B {
  def foo = 123
  @deprecated def bar = 456
}
