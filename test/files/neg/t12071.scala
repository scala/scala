// scalac: -Werror -Xlint

class C {
  def `c c`(n: Int): Int = n + 1
}

// backticked operator is candidate for multiline infix,
// but backticked value is an innocent bystander.
//
class t12071 {
  def c: C = ???
  def i: Int = 42
  def `n n`: Int = 17
  def f = c
    `c c` i
  def g = i +
    `n n`
  def basic =
      1
    + 2
}

object C {
  def x = 42
    + 1

  def y = 1 +
    `test-1` + `test-2`

  def z = 2
    `compareTo` (2 - 1)

  def `test-1`: Int = 23
  def `test-2`: Int = 42
  def compareTo(x: Int) = println("lol")

  var `test-3`: List[Int] = Nil

  // since ++ is not unary, test-3 is not taken as an operator; this test doesn't fix y above.
  def yy = List.empty[Int] ++
    `test-3` ++ `test-3`
}
