
class C {
  def f(i: Int, s: String) = s * i
  def g = (i: String, s) => f(i, s)
}

class D {
  def f(i: Int, s: String) = s * i
  def g = (i: Junk, s) => f(i, s)
}
