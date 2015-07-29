class Test(val x: Long) {
  def sameDirection(y: Long): Boolean =
    (y == 0 || x == 0 || ((y > 0) == (x > 0)))
}
 
object Test {
  def main(args: Array[String]) {
    val b = new Test(1L)
    assert(!b.sameDirection(-1L))
  }
}
