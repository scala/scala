//> using options -Xsource:3

class C {
  def f = {
    x: Int => x * 2
  }
  def g = (x: Int) => x * 2
}
