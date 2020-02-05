// scalac: -Xsource:3

class K { def x(y: Int) = 0 }

class Test {
  def ok = {
    (new K)
    `x` 42
  }
}
