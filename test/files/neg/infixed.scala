//> using options -Xsource:3

class K { def x(y: Int) = 0 }

class Test {
  def bad = {
    (new K)
    x 42
  }
}
