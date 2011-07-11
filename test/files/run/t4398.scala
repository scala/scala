

object Test {
  def main(args: Array[String]) {
    val x = 1 to 10 toSet
    val y = x + 5
    val z = y - 154321
    assert(x eq y)
    assert(x eq z)
  }
}
