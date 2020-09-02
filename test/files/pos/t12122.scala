
class C {
  def f(i: Int, s: String) = s * i
  def g = (i: Int, s) => f(i, s)
}
