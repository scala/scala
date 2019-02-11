object Client {
  def test= {
    Test.Outer.Nested.sig
    Test.Outer.Nested.sig1
    Test.Outer.Nested.sig2
    val o = new Test.Outer
    new o.Nested1().sig
    new o.Nested1().sig1
    new o.Nested1().sig2
  }

  def test1 = {
    val t = new Test
    val o = new t.Outer1
    new o.Nested1().sig
    new o.Nested1().sig1
    new o.Nested1().sig2
  }
}
