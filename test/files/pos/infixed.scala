//> using options -Xsource:3 -Xsource-features:leading-infix

class K { def x(y: Int) = 0 }

class Test {
  def ok = {
    (new K)
    `x` 42
  }
}
