
//> using options -Werror -Xlint -Xsource:3

class C {
  def f = 42 + (x = 1)
  def multi(x: Int, y: Int): Int = x + y
  def g = new C() `multi` (x = 42, y = 27)
}
