// scalac: -Werror -Xlint -Xsource:3

class C {
  def `c c`(n: Int): Int = n + 1
}

// backticked operator is candidate for multiline infix,
// but backticked value is an innocent bystander.
//
class t12071 {
  def c: C = new C
  def i: Int = 42
  def `n n`: Int = 27
  def f = c
    `c c` i
  def g = i +
    `n n`
  def basic =
      1
    + 2
}

object Test extends App {
  val t = new t12071
  assert(t.f == 43)
  assert(t.g == 69)
  assert(t.basic == 3)
}
