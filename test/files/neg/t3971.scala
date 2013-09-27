class A {
  def f(x: String) = x
  def g(x: String)(y: String): Int = x.length + y.length
  def h(x: String) = x.length

  f(g("abc")("def")) // g returns Int, needs String
  f(5)
  f(h("abc"))

  // a perverse piece of code from a perverse coder
  ({"ab".reverse; "ba".equals})(0): String
}
