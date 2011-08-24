object bug {
  def foobar[c]: Int = {
    class Foo { def foo: Bar = new Bar(); }
    class Bar { def bar: c   = bar; }
    0
  }
}
