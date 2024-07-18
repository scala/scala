//> using options -Werror -Wconf:msg=lambda-parens:s -Xsource:3

class C {
  def f = {
    x: Int => x * 2
  }
  def g = (x: Int) => x * 2
}
