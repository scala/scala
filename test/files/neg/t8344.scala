object t {
  def f(x: Object) = 1
  def f(x: String*) = 2
}

class Test {
  t.f("")
}
