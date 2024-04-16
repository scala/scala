//> using options -Werror -Xlint -Xmigration:2.13

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

  def yy = 1
  /* fails in scala 3
    +
    `test-1`
    +
    `test-2`
   */
}

object Test extends App {
  println(C.x)
  println(C.y)
  println(C.z)
  println(C.yy)
}
