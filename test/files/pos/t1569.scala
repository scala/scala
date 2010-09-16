object Bug {
  class C { type T }
  def foo(x: Int)(y: C)(z: y.T) {}
  foo(3)(new C { type T = String })("hello")
}